#!/usr/bin/env python3
"""
LispOS Development & Verification Harness
Provides:
  - Background QEMU daemon management (start, stop, status)
  - Remote Common Lisp evaluation via SLY/Slynk (TCP port 4005)
  - Framebuffer visual capture via QMP screendump + conversion to PNG
  - Interactive REPL and log viewer
"""

import sys
import os
import time
import socket
import json
import signal
import subprocess
import argparse
import platform
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
BUILD_DIR = PROJECT_ROOT / "build"
PID_FILE = BUILD_DIR / "qemu.pid"
QMP_SOCK = BUILD_DIR / "qmp.sock"
SERIAL_LOG = BUILD_DIR / "serial.log"
INITRAMFS = BUILD_DIR / "initramfs.cpio.gz"
DEFAULT_SCREENSHOT = BUILD_DIR / "screenshot.png"

DEFAULT_HOST = "127.0.0.1"
DEFAULT_SLYNK_PORT = 4005
DEFAULT_VNC_PORT = 5900


def find_default_kernel():
    uname_r = platform.release()
    candidates = [
        Path(f"/lib/modules/{uname_r}/vmlinuz"),
        Path("/boot/vmlinuz-linux"),
        PROJECT_ROOT / "vmlinuz",
    ]
    for c in candidates:
        if c.is_file():
            return str(c)
    return "/boot/vmlinuz-linux"


# ==========================================
# QEMU Management
# ==========================================

def get_running_pid():
    if not PID_FILE.exists():
        return None
    try:
        pid = int(PID_FILE.read_text().strip())
        os.kill(pid, 0)
        return pid
    except (ValueError, OSError):
        return None


def start_qemu(kernel=None, headless=True, mem="1024M", qemu_bin="qemu-system-x86_64", timeout=25):
    pid = get_running_pid()
    if pid:
        print(f"[LispOS] QEMU is already running with PID {pid}.")
        return 0

    BUILD_DIR.mkdir(parents=True, exist_ok=True)

    if not INITRAMFS.exists():
        print(f"[LispOS] Initramfs not found. Building {INITRAMFS}...")
        res = subprocess.run(["make", "initramfs"], cwd=PROJECT_ROOT)
        if res.returncode != 0:
            print("[LispOS Error] Failed to build initramfs.", file=sys.stderr)
            return 1

    if not kernel:
        kernel = find_default_kernel()
    if not os.path.exists(kernel):
        print(f"[LispOS Error] Kernel image not found at: {kernel}", file=sys.stderr)
        return 1

    # Remove stale QMP socket if left behind
    if QMP_SOCK.exists():
        try:
            QMP_SOCK.unlink()
        except OSError:
            pass

    cmd = [
        qemu_bin,
        "-enable-kvm",
        "-cpu", "host",
        "-m", mem,
        "-kernel", kernel,
        "-initrd", str(INITRAMFS),
        "-append", "console=ttyS0 rdinit=/System/Core/Init quiet",
        "-vga", "std",
        "-serial", f"file:{SERIAL_LOG}",
        "-nic", f"user,hostfwd=tcp::{DEFAULT_SLYNK_PORT}-:{DEFAULT_SLYNK_PORT}",
        "-qmp", f"unix:{QMP_SOCK},server,nowait",
        "-pidfile", str(PID_FILE),
        "-daemonize"
    ]

    if headless:
        cmd.extend(["-display", "none", "-vnc", "127.0.0.1:0"])
    else:
        cmd.extend(["-vnc", "127.0.0.1:0"])


    print(f"[LispOS] Launching QEMU daemon (Kernel: {kernel}, Mem: {mem}, Headless: {headless})...")
    res = subprocess.run(cmd, cwd=PROJECT_ROOT)
    if res.returncode != 0:
        print(f"[LispOS Error] QEMU failed to start (exit code {res.returncode})", file=sys.stderr)
        return res.returncode

    # Wait for QMP socket and PID file
    pid = None
    start_time = time.time()
    while time.time() - start_time < 5.0:
        pid = get_running_pid()
        if pid and QMP_SOCK.exists():
            break
        time.sleep(0.2)

    if not pid:
        print("[LispOS Error] QEMU process PID not detected after launch.", file=sys.stderr)
        return 1

    print(f"[LispOS] QEMU daemon started successfully (PID: {pid}).")
    print(f"[LispOS] QMP Monitor socket: {QMP_SOCK}")
    print(f"[LispOS] Serial log file: {SERIAL_LOG}")

    # Wait for SLYNK port 4005 to become active
    print(f"[LispOS] Waiting for LispOS userland and SLYNK server on port {DEFAULT_SLYNK_PORT}...")
    slynk_ready = False
    while time.time() - start_time < timeout:
        try:
            with socket.create_connection((DEFAULT_HOST, DEFAULT_SLYNK_PORT), timeout=1.0):
                slynk_ready = True
                break
        except (ConnectionRefusedError, OSError):
            time.sleep(0.5)

    if slynk_ready:
        print(f"[LispOS] LispOS booted! SLYNK REPL active on {DEFAULT_HOST}:{DEFAULT_SLYNK_PORT}.")
        return 0
    else:
        print(f"[LispOS Warning] SLYNK port not responding after {timeout}s. Check {SERIAL_LOG} for details.")
        return 0


def stop_qemu(timeout=10):
    pid = get_running_pid()
    if not pid:
        print("[LispOS] No running QEMU instance detected.")
        if PID_FILE.exists():
            PID_FILE.unlink(missing_ok=True)
        if QMP_SOCK.exists():
            QMP_SOCK.unlink(missing_ok=True)
        return 0

    print(f"[LispOS] Stopping QEMU daemon (PID: {pid})...")

    # Try graceful shutdown via QMP first
    if QMP_SOCK.exists():
        try:
            qmp_execute({"execute": "quit"})
        except Exception:
            pass

    # If still alive, send SIGTERM
    start_time = time.time()
    while time.time() - start_time < timeout:
        try:
            os.kill(pid, 0)
            time.sleep(0.3)
        except OSError:
            break
    else:
        # Force kill
        try:
            os.kill(pid, signal.SIGKILL)
        except OSError:
            pass

    if PID_FILE.exists():
        PID_FILE.unlink(missing_ok=True)
    if QMP_SOCK.exists():
        QMP_SOCK.unlink(missing_ok=True)

    print("[LispOS] QEMU daemon stopped.")
    return 0


def status_qemu():
    pid = get_running_pid()
    if not pid:
        print("[LispOS Status] QEMU is NOT running.")
        return 1

    print(f"[LispOS Status] QEMU is RUNNING (PID: {pid})")

    # Check QMP socket
    qmp_ok = False
    if QMP_SOCK.exists():
        try:
            resp = qmp_execute({"execute": "query-status"})
            qmp_ok = "return" in resp
        except Exception:
            qmp_ok = False
    print(f"  - QMP Socket ({QMP_SOCK}): {'ACTIVE' if qmp_ok else 'UNAVAILABLE'}")

    # Check SLYNK port
    slynk_ok = False
    try:
        with socket.create_connection((DEFAULT_HOST, DEFAULT_SLYNK_PORT), timeout=1.0):
            slynk_ok = True
    except Exception:
        slynk_ok = False
    print(f"  - SLYNK REPL ({DEFAULT_HOST}:{DEFAULT_SLYNK_PORT}): {'LISTENING' if slynk_ok else 'NOT REACHABLE'}")

    # Check VNC port
    vnc_ok = False
    try:
        with socket.create_connection((DEFAULT_HOST, DEFAULT_VNC_PORT), timeout=1.0):
            vnc_ok = True
    except Exception:
        vnc_ok = False
    print(f"  - VNC Display ({DEFAULT_HOST}:{DEFAULT_VNC_PORT}): {'LISTENING' if vnc_ok else 'NOT REACHABLE'}")

    print(f"  - Serial Log: {SERIAL_LOG} ({SERIAL_LOG.stat().st_size if SERIAL_LOG.exists() else 0} bytes)")


    return 0


# ==========================================
# QMP Protocol Client
# ==========================================

def qmp_execute(command_obj):
    """Execute a QMP command over unix domain socket and return JSON response."""
    if not QMP_SOCK.exists():
        raise RuntimeError(f"QMP socket not found at {QMP_SOCK}. Is QEMU running?")

    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.settimeout(5.0)
    s.connect(str(QMP_SOCK))

    try:
        # Read greeting
        f = s.makefile("r", encoding="utf-8")
        greeting = f.readline()
        # Capabilities handshake
        s.sendall(b'{"execute": "qmp_capabilities"}\n')
        ack = f.readline()

        # Send command
        cmd_str = json.dumps(command_obj) + "\n"
        s.sendall(cmd_str.encode("utf-8"))

        # Read response, filtering out async events
        while True:
            line = f.readline()
            if not line:
                break
            resp = json.loads(line)
            if "event" in resp:
                continue
            return resp
        return {}
    finally:
        s.close()


def capture_screenshot(output_path=None):
    if output_path is None:
        output_path = DEFAULT_SCREENSHOT
    output_path = Path(output_path).resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Flush DRM framebuffer first if Slynk is available
    try:
        flush_client = SlynkClient(timeout=1.0)
        flush_client.connect()
        flush_client.eval("(lispos.drm:flush-screen)", package="LISPOS.DRM")
        flush_client.close()
    except Exception:
        pass

    temp_ppm = BUILD_DIR / f"temp_{os.getpid()}_{int(time.time()*1000)}.ppm"

    try:
        # Request screendump from QEMU
        resp = qmp_execute({

            "execute": "screendump",
            "arguments": {"filename": str(temp_ppm)}
        })


        if "error" in resp:
            raise RuntimeError(f"QMP screendump failed: {resp['error']}")

        if not temp_ppm.exists() or temp_ppm.stat().st_size == 0:
            raise RuntimeError("Screendump generated an empty or non-existent file.")

        # Convert PPM to PNG using magick or convert or ffmpeg
        convert_cmd = None
        for cand in ["magick", "convert", "ffmpeg"]:
            if shutil_which(cand):
                convert_cmd = cand
                break

        if convert_cmd == "magick" or convert_cmd == "convert":
            cmd = [convert_cmd, str(temp_ppm), str(output_path)]
        elif convert_cmd == "ffmpeg":
            cmd = ["ffmpeg", "-y", "-i", str(temp_ppm), str(output_path)]
        else:
            # Fallback pure python conversion
            ppm_to_png_fallback(temp_ppm, output_path)
            cmd = None

        if cmd:
            res = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            if res.returncode != 0:
                # Fallback to python
                ppm_to_png_fallback(temp_ppm, output_path)

        if not output_path.exists():
            raise RuntimeError(f"Failed to generate destination PNG: {output_path}")

        print(f"[LispOS] Screenshot captured successfully ({output_path.stat().st_size} bytes):")
        print(f"![LispOS Screen](file://{output_path})")
        return str(output_path)

    finally:
        if temp_ppm.exists():
            temp_ppm.unlink(missing_ok=True)


def shutil_which(cmd):
    import shutil
    return shutil.which(cmd)


def ppm_to_png_fallback(ppm_path, png_path):
    """Pure Python PPM (P6) to uncompressed PNG fallback using zlib."""
    import zlib
    import struct

    with open(ppm_path, "rb") as f:
        magic = f.readline().strip()
        if magic != b"P6":
            raise ValueError(f"Unsupported PPM magic: {magic}")
        # Skip comments
        line = f.readline()
        while line.startswith(b"#"):
            line = f.readline()
        dims = line.strip().split()
        width, height = int(dims[0]), int(dims[1])
        maxval = int(f.readline().strip())
        raw_pixels = f.read()

    # Create scanlines with filter byte 0 (None)
    stride = width * 3
    scanlines = bytearray()
    for y in range(height):
        scanlines.append(0)
        scanlines.extend(raw_pixels[y * stride : (y + 1) * stride])

    compressed = zlib.compress(bytes(scanlines), 9)

    def chunk(chunk_type, data):
        length = len(data)
        crc = zlib.crc32(chunk_type + data) & 0xFFFFFFFF
        return struct.pack(">I", length) + chunk_type + data + struct.pack(">I", crc)

    png_data = bytearray(b"\x89PNG\r\n\x1a\n")
    ihdr = struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)
    png_data.extend(chunk(b"IHDR", ihdr))
    png_data.extend(chunk(b"IDAT", compressed))
    png_data.extend(chunk(b"IEND", b""))

    with open(png_path, "wb") as f:
        f.write(png_data)


# ==========================================
# SLY / Slynk Client
# ==========================================

class SlynkClient:
    def __init__(self, host=DEFAULT_HOST, port=DEFAULT_SLYNK_PORT, timeout=10.0):
        self.host = host
        self.port = port
        self.timeout = timeout
        self.sock = None
        self.req_id = 0

    def connect(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.settimeout(self.timeout)
        self.sock.connect((self.host, self.port))

    def close(self):
        if self.sock:
            try:
                self.sock.close()
            except Exception:
                pass
            self.sock = None

    def _send_packet(self, msg_str):
        raw = msg_str.encode("utf-8")
        header = f"{len(raw):06x}".encode("ascii")
        self.sock.sendall(header + raw)

    def _read_packet(self):
        hdr = self.sock.recv(6)
        if not hdr:
            return None
        length = int(hdr.decode("ascii"), 16)
        data = bytearray()
        while len(data) < length:
            chunk = self.sock.recv(length - len(data))
            if not chunk:
                break
            data.extend(chunk)
        return data.decode("utf-8")

    def eval(self, form_str, package="COMMON-LISP-USER"):
        """
        Evaluate form in running LispOS safely wrapped in handler-case.
        Returns: (output_stdout, return_values, is_error)
        """
        if not self.sock:
            self.connect()

        self.req_id += 1
        req_id = self.req_id

        # Wrap in handler-case to catch conditions and avoid entering interactive SLDB debugger
        wrapped = f"""(handler-case
  (let ((val (multiple-value-list (progn {form_str}))))
    (format nil "~{{~S~^, ~}}" val))
  (error (c)
    (format nil "[LispOS Condition] ~A" c)))"""

        # Escape for Common Lisp string literal inside slynk:eval-and-grab-output
        escaped = wrapped.replace("\\", "\\\\").replace('"', '\\"')
        req = f'(:emacs-rex (slynk:eval-and-grab-output "{escaped}") "{package}" t {req_id})\n'


        self._send_packet(req)

        stdout_accum = []
        while True:
            msg = self._read_packet()
            if not msg:
                break

            # Handle async printed output from Lisp process
            if msg.startswith("(:write-string"):
                # Format: (:write-string "text" ...)
                parts = msg.split('"', 2)
                if len(parts) >= 2:
                    stdout_accum.append(parts[1])
                continue

            # Check for return message
            if msg.startswith("(:return"):
                # Parse return message: (:return (:ok (output result)) id)
                res_output = "".join(stdout_accum)
                # Extract output and value from string
                return self._parse_return_msg(msg, res_output)

        return "".join(stdout_accum), "No response from SLYNK", True

    def _parse_return_msg(self, msg, stdout_accum):
        # Look for (:ok (out val))
        if "(:ok (" in msg:
            # We can extract the strings
            try:
                # Find start of (:ok (
                idx = msg.index("(:ok (") + 6
                # Extract the two strings: output and result
                # String 1: standard-output printed during eval
                # String 2: return value or error
                # Let's parse with simple state machine
                s = msg[idx:]
                strs = []
                in_str = False
                cur = []
                esc = False
                for ch in s:
                    if esc:
                        cur.append(ch)
                        esc = False
                    elif ch == "\\":
                        esc = True
                    elif ch == '"':
                        if in_str:
                            strs.append("".join(cur))
                            cur = []
                            in_str = False
                            if len(strs) == 2:
                                break
                        else:
                            in_str = True
                    elif in_str:
                        cur.append(ch)

                out = stdout_accum + (strs[0] if len(strs) > 0 else "")
                val = strs[1] if len(strs) > 1 else ""
                is_err = val.startswith("[LispOS Condition]")
                return out, val, is_err
            except Exception as e:
                return stdout_accum, msg, False
        elif "(:abort" in msg:
            return stdout_accum, f"Evaluation aborted: {msg}", True
        else:
            return stdout_accum, msg, False


def eval_form(form_str, package="COMMON-LISP-USER"):
    client = SlynkClient()
    try:
        client.connect()
    except Exception as e:
        print(f"[LispOS Error] Unable to connect to SLYNK server on {DEFAULT_HOST}:{DEFAULT_SLYNK_PORT}: {e}", file=sys.stderr)
        print("[LispOS] Is QEMU running? Try: ./tools/harness.py start", file=sys.stderr)
        return 1

    try:
        out, val, is_err = client.eval(form_str, package=package)
        if out:
            print(out, end="" if out.endswith("\n") else "\n")
        print(val)
        return 1 if is_err else 0
    finally:
        client.close()


def interactive_repl(package="COMMON-LISP-USER"):
    client = SlynkClient()
    try:
        client.connect()
    except Exception as e:
        print(f"[LispOS Error] Unable to connect to SLYNK server on {DEFAULT_HOST}:{DEFAULT_SLYNK_PORT}: {e}", file=sys.stderr)
        return 1

    print("Connected to LispOS Interactive Remote REPL (Slynk).")
    print("Type Common Lisp expressions, or 'quit' / Ctrl-D to exit.\n")

    current_pkg = package
    try:
        while True:
            try:
                line = input(f"{current_pkg}> ")
            except EOFError:
                print("\nExiting REPL.")
                break

            form = line.strip()
            if not form:
                continue
            if form in ("quit", "exit", ":q"):
                break

            # Handle package switch shortcut: (in-package ...)
            if form.startswith("(in-package") or form.startswith("(IN-PACKAGE"):
                parts = form.replace("(", "").replace(")", "").split()
                if len(parts) >= 2:
                    current_pkg = parts[1].lstrip(":").upper()

            out, val, is_err = client.eval(form, package=current_pkg)
            if out:
                print(out, end="" if out.endswith("\n") else "\n")
            print(val)
    finally:
        client.close()
    return 0


# ==========================================
# CLI Entrypoint
# ==========================================

def main():
    parser = argparse.ArgumentParser(description="LispOS Development & Verification Harness")
    subparsers = parser.add_subparsers(dest="command", required=True)

    # start
    p_start = subparsers.add_parser("start", help="Start LispOS in background QEMU daemon")
    p_start.add_argument("--kernel", default=None, help="Path to vmlinuz kernel")
    p_start.add_argument("--gui", action="store_true", help="Launch with graphical QEMU window")
    p_start.add_argument("--mem", default="1024M", help="Memory allocation (default: 1024M)")

    # stop
    subparsers.add_parser("stop", help="Stop running LispOS QEMU daemon")

    # status
    subparsers.add_parser("status", help="Check status of QEMU daemon, SLYNK server, and QMP")

    # eval
    p_eval = subparsers.add_parser("eval", help="Evaluate a Common Lisp expression on live LispOS")
    p_eval.add_argument("expression", nargs="?", default=None, help="Lisp form to evaluate")
    p_eval.add_argument("--expr", default=None, help="Alternative flag for lisp expression")
    p_eval.add_argument("--pkg", default="COMMON-LISP-USER", help="Target package (default: COMMON-LISP-USER)")

    # screenshot
    p_ss = subparsers.add_parser("screenshot", help="Capture visual framebuffer screenshot to PNG")
    p_ss.add_argument("output", nargs="?", default=None, help="Output PNG path (default: build/screenshot.png)")

    # repl
    p_repl = subparsers.add_parser("repl", help="Start interactive remote CLI REPL")
    p_repl.add_argument("--pkg", default="COMMON-LISP-USER", help="Initial package")

    # log
    p_log = subparsers.add_parser("log", help="Display serial console log")
    p_log.add_argument("-n", "--lines", type=int, default=50, help="Number of trailing lines to view")
    p_log.add_argument("-f", "--follow", action="store_true", help="Follow log in real time")

    args = parser.parse_args()

    if args.command == "start":
        return start_qemu(kernel=args.kernel, headless=not args.gui, mem=args.mem)
    elif args.command == "stop":
        return stop_qemu()
    elif args.command == "status":
        return status_qemu()
    elif args.command == "eval":
        expr = args.expression or args.expr
        if not expr:
            if not sys.stdin.isatty():
                expr = sys.stdin.read().strip()
            else:
                print("Error: No expression provided. Usage: harness.py eval \"(+ 1 2)\"", file=sys.stderr)
                return 1
        return eval_form(expr, package=args.pkg)
    elif args.command == "screenshot":
        try:
            capture_screenshot(args.output)
            return 0
        except Exception as e:
            print(f"[LispOS Error] Screenshot capture failed: {e}", file=sys.stderr)
            return 1
    elif args.command == "repl":
        return interactive_repl(package=args.pkg)
    elif args.command == "log":
        if not SERIAL_LOG.exists():
            print(f"Log file {SERIAL_LOG} does not exist.", file=sys.stderr)
            return 1
        if args.follow:
            subprocess.run(["tail", "-f", "-n", str(args.lines), str(SERIAL_LOG)])
        else:
            subprocess.run(["tail", "-n", str(args.lines), str(SERIAL_LOG)])
        return 0


if __name__ == "__main__":
    sys.exit(main())
