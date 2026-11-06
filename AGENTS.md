# AGENTS.md - Developer & Agent Guide for LispOS

This guide provides architectural context, development workflows, pitfalls, and verification instructions for AI agents and human developers working on **LispOS**.

---

## 1. Project Overview & Vision

**LispOS** is a bare-metal, monolithic Common Lisp operating system environment where **Steel Bank Common Lisp (SBCL) runs directly as PID 1** on top of the Linux kernel.

### Core Architectural Principles
1. **No GNU/POSIX Userland**: Standard Unix utilities (`systemd`, `bash`, `coreutils`, `busybox`, glibc dynamic NSS switchers) do not exist in the root filesystem. All OS responsibilities (init, mounting, networking, hardware drivers, process supervision, REPL, windowing) are written in Common Lisp.
2. **Clean Non-FHS Directory Structure**:
   ```
   /
   ├── System/
   │   ├── Core/                # Monolithic executable (/System/Core/Init)
   │   ├── Libraries/           # Minimal dynamic runtime (ld-linux, libc, libm, libdrm)
   │   │   └── Modules/         # In-tree hardware kernel modules (.ko)
   │   ├── Kernel/              # Virtual filesystem mountpoints
   │   │   ├── Devices/         # devtmpfs (instead of /dev)
   │   │   ├── Processes/       # procfs (instead of /proc)
   │   │   └── Hardware/        # sysfs (instead of /sys)
   │   ├── Runtime/             # Ephemeral tmpfs (sockets, PID files)
   │   ├── Source/LispOS/       # OS source tree mounted into image for live inspection
   │   └── Configuration/      # System settings and boot configs
   ├── Applications/            # User applications and subsystems
   ├── Library/                 # Shared resources (fonts, assets)
   └── Users/                   # User home directories (/Users/Admin)
   ```
3. **Hardware Driver Initialization**:
   - Kernel modules are loaded dynamically at boot time by PID 1 using the Linux `finit_module(2)` syscall (`lispos.sys:load-kernel-module`).
   - `bochs.ko` provides the DRM/KMS graphics device on `/System/Kernel/Devices/dri/card0`.
   - `e1000.ko` provides the network interface (`eth0`).
4. **Direct DRM/KMS Graphics**:
   - PID 1 opens `/System/Kernel/Devices/dri/card0`, allocates a dumb buffer, sets up the CRTC (`1280x800`), mmaps the framebuffer SAP, and renders directly.
   - Updates must be flushed via `(lispos.drm:flush-screen)` which invokes `drmModeDirtyFB` to notify the display hardware.
5. **Multi-Threaded Concurrency**:
   - Input subsystem (`lispos.input`) runs an evdev event loop in a dedicated SBCL thread.
   - SLY/Swank remote server runs on TCP port `4005`.
   - PID 1 main thread runs an immortal crash-proof supervisor loop (`reap-zombies` heartbeat).

---

## 2. Critical Pitfalls & Must-Know Rules

### A. The SBCL Core Trailer & `patchelf` Trap
> [!IMPORTANT]
> SBCL executable binaries (`build/Init`) contain a 32-byte core trailer at the very end of the file consisting of `CORE_MAGIC` (`#x5342434C`) and the byte offset of the core image.
>
> When `patchelf` is run on the binary to set the dynamic interpreter and RPATH to `/System/Libraries`, it appends new ELF section data to the end of the file, displacing the 32-byte trailer. This causes SBCL runtime startup to fail with:
> ```
> fatal error: can't find core file at ...
> ```
> **The Fix**: The `Makefile` extracts the original 32-byte trailer from `build/Init` and appends it back to `build/initramfs/System/Core/Init` immediately after `patchelf`.
> **Never modify or bypass the trailer restoration step in `Makefile`!**

### B. SBCL Scripting & Debugger Traps
> [!WARNING]
> When executing `sbcl` commands from shell scripts, python, or automation tools, **always pass `--non-interactive`** (or `--disable-debugger`). Without this flag, any unhandled error drops SBCL into an interactive prompt reading stdin, which will hang automated tasks indefinitely.

### C. SLDB / Remote Evaluation Immunity
In [`tools/harness.py`](file:///home/meghdip/Projects/LispOS/tools/harness.py), forms sent over SLYNK port 4005 are wrapped in `(handler-case (let ((val (multiple-value-list (progn <EXPR>)))) ...) (error (c) ...))` so that runtime errors in Lisp forms return descriptive error strings rather than suspending the worker thread in SLDB.

### D. DRM Dumb Buffer Page Flushing
When modifying framebuffer pixels via `(sb-sys:sap-ref-32 sap ...)`, `(lispos.drm:draw-rect ...)`, etc., you **must** call `(lispos.drm:flush-screen)` to invoke `drmModeDirtyFB`. Emulated display adapters (such as Bochs) use a shadow buffer in host RAM and only copy modified lines to the display plane when notified.

---

## 3. Standard Development & Verification Workflow

Agents working on LispOS must use the provided daemon harness to verify changes visually and programmatically.

### Harness Commands

| Command | Action |
|---|---|
| `make start` | Builds `initramfs` (if needed) and starts LispOS in background QEMU daemon |
| `make status` | Checks process PID, QMP socket, SLY port (4005), and VNC port (5900) |
| `make eval EXPR='...'` | Evaluates a Common Lisp form on live LispOS via Slynk and prints result |
| `make screenshot` | Flushes framebuffer, screendumps via QMP, and saves `build/screenshot.png` |
| `make repl` | Opens interactive remote CLI REPL connected to running LispOS |
| `make log` | Views serial console log (`build/serial.log`) |
| `make stop` | Gracefully shuts down QEMU daemon |

### Standard Iteration Loop for Agents

1. **Modify Code**: Edit files in [`src/`](file:///home/meghdip/Projects/LispOS/src/).
2. **Rebuild Image & Initramfs**:
   ```bash
   make initramfs
   ```
3. **Restart Daemon**:
   ```bash
   ./tools/harness.py stop && ./tools/harness.py start
   ```
4. **Verify Boot & Status**:
   ```bash
   make status
   ```
5. **Interactive Testing via Eval**:
   ```bash
   ./tools/harness.py eval "(mapcar #'namestring (directory #P\"/*\"))"
   ```
6. **Visual Framebuffer Verification**:
   ```bash
   make screenshot
   ```
   Inspect the resulting image with `view_file` on `build/screenshot.png` or embed it in conversation responses:
   ```markdown
   ![LispOS Screen](file:///home/meghdip/Projects/LispOS/build/screenshot.png)
   ```

---

## 4. Codebase Navigation

| File | Purpose |
|---|---|
| [`lispos.asd`](file:///home/meghdip/Projects/LispOS/lispos.asd) | ASDF system definition and subsystem dependencies (cffi, slynk, bordeaux-threads). |
| [`build-image.lisp`](file:///home/meghdip/Projects/LispOS/build-image.lisp) | Script to load Quicklisp, load `:lispos`, configure CFFI, and dump monolithic `build/Init`. |
| [`Makefile`](file:///home/meghdip/Projects/LispOS/Makefile) | Full pipeline: SBCL compilation, non-FHS directory tree, library bundling, patchelf + trailer patch, QEMU runner, harness integration. |
| [`src/package.lisp`](file:///home/meghdip/Projects/LispOS/src/package.lisp) | Package definitions (`lispos.paths`, `lispos.sys`, `lispos.drm`, `lispos.input`, `lispos.repl`, `lispos.sandbox`, `lispos.init`). |
| [`src/paths.lisp`](file:///home/meghdip/Projects/LispOS/src/paths.lisp) | Non-FHS pathname constants and logical pathname translations (`SYS:`, `CORE:`, etc.). |
| [`src/syscalls.lisp`](file:///home/meghdip/Projects/LispOS/src/syscalls.lisp) | Direct libc/Linux syscall CFFI bindings (`mount`, `umount`, `finit_module`, ioctl, signal, reboot). |
| [`src/drm.lisp`](file:///home/meghdip/Projects/LispOS/src/drm.lisp) | Dumb-buffer KMS graphics, `drmModeDirtyFB` page flusher, 8x8 font engine, drawing primitives. |
| [`src/input.lisp`](file:///home/meghdip/Projects/LispOS/src/input.lisp) | Background thread listening on `/System/Kernel/Devices/input/event*` with non-blocking libc `read`. |
| [`src/repl.lisp`](file:///home/meghdip/Projects/LispOS/src/repl.lisp) | SLY/Slynk server on `0.0.0.0:4005` and fallback TTY serial REPL loop. |
| [`src/sandbox.lisp`](file:///home/meghdip/Projects/LispOS/src/sandbox.lisp) | Restricted `safe-eval` evaluator with `*read-eval* nil` for unprivileged applications. |
| [`src/init.lisp`](file:///home/meghdip/Projects/LispOS/src/init.lisp) | Monolithic PID 1 entry point (`boot-os`), global crash guard, boot splash renderer, supervisor heartbeat loop. |
| [`tools/harness.py`](file:///home/meghdip/Projects/LispOS/tools/harness.py) | Python daemon harness providing QEMU lifecycle management, SLYNK wire protocol client, and QMP screendump converter. |

---

## 5. Remote Access Over SSH

When working remotely over SSH, tunnel both VNC (port 5900) and SLY (port 4005):
```bash
ssh -L 5900:127.0.0.1:5900 -L 4005:127.0.0.1:4005 <user>@<remote-ip>
```
- **Live GUI:** Connect any VNC viewer (`vncviewer localhost:5900`, Remmina, TigerVNC, macOS Screen Sharing `vnc://localhost:5900`).
- **Live REPL:** In local Emacs, run `M-x sly-connect RET localhost RET 4005`.
