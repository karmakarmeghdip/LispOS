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

## 4. Codebase Architecture & Subsystems

LispOS source code is strictly structured into modular subsystems under [`src/`](file:///home/meghdip/Projects/LispOS/src/). **Never dump loose `.lisp` files directly into `src/`.** Every component belongs to a designated subsystem with its own localized `package.lisp`:

```
src/
├── core/                      # Low-level platform & runtime
│   ├── package.lisp           # lispos.paths, lispos.sys
│   ├── paths.lisp             # Non-FHS root paths & logical pathname mappings
│   └── syscalls.lisp          # Linux syscalls (mount, umount, finit_module, ioctl)
├── hal/                       # Hardware Abstraction Layer
│   ├── drm/                   # KMS graphics driver (ioctl.lisp, framebuffer.lisp)
│   └── input/                 # evdev reader (keycodes.lisp, evdev.lisp)
├── graphics/                  # 2D Rendering & Typography
│   ├── package.lisp           # lispos.display, lispos.font
│   ├── surface.lisp           # Double-buffered display backbuffer & dirty rect damage tracker
│   └── font.lisp              # FreeType2 rasterizer & glyph bitmap cache
├── ui/                        # Tiling Window Manager & Compositor
│   ├── package.lisp           # lispos.buffer, lispos.window, lispos.picker, lispos.terminal
│   ├── buffer.lisp            # Buffer protocol (lines, cursor point, modes)
│   ├── window-tree.lisp       # Binary split-tree layout & directional focus
│   ├── picker.lisp            # Centered floating fuzzy finder & command palette
│   └── terminal.lisp          # Window tree renderer, modeline & compositor event loop
├── shell/                     # Interactive Lisp / CLI Shell
│   ├── package.lisp           # lispos.shell
│   ├── state.lisp             # Shell engine struct & configuration
│   ├── parser.lisp            # Tokenizer, paren balance validator & path resolver
│   ├── line-editor.lisp       # Structural paren pairing, cursor nav, kill-ring
│   ├── history.lisp           # Command history ring & uncommitted scratch retention
│   ├── commands/              # Shell built-in command handlers
│   │   ├── registry.lisp      # define-shell-command macro & registry table
│   │   ├── filesystem.lisp    # ls, cd, pwd, cat, touch, mkdir, rm
│   │   └── system.lisp        # ps, free, whoami, reboot, poweroff, help
│   └── engine.lisp            # Dual-mode evaluator (Lisp form vs shell command)
└── supervisor/                # PID 1 Init & Daemon Services
    ├── package.lisp           # lispos.sandbox, lispos.repl, lispos.init
    ├── sandbox.lisp           # Restricted safe-eval environment
    ├── repl.lisp              # Serial console REPL (ttyS0) & SLYNK TCP server
    └── init.lisp              # PID 1 entry point & immortal crash-guard loop
```

When implementing new features or adding new files:
1. Locate the file within the proper subsystem directory.
2. Export relevant symbols from that subsystem's `package.lisp`.
3. Register the file in [`lispos.asd`](file:///home/meghdip/Projects/LispOS/lispos.asd) under the corresponding subsystem module component.

---

## 5. SBCL Type Safety & Performance Conventions

Instead of heavy external type systems (like Coalton) that introduce large dependency trees and conflict with low-level CFFI/SAP programming, **LispOS leverages SBCL's native compile-time type system**. Future threads must adhere to these conventions for safety and speed:

### A. Strongly Typed `defstruct`
Always specify slot types with default values:
```lisp
(defstruct (window (:constructor %make-window))
  (id (gensym "WIN-") :type symbol)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (width 1280 :type fixnum)
  (height 800 :type fixnum)
  (split-ratio 0.5 :type single-float))
```

### B. Algebraic Union Types with `deftype`
Model domain variants with `deftype` and `member`:
```lisp
(deftype split-direction ()
  '(member :horizontal :vertical))

(deftype pixel-argb ()
  '(unsigned-byte 32))
```

### C. Function Signatures with `declaim (ftype ...)`
Proclaim function signatures for public API boundaries and performance-critical functions:
```lisp
(declaim (ftype (function (fixnum fixnum fixnum fixnum pixel-argb) null) draw-rect))
(defun draw-rect (x y w h color)
  (declare (optimize (speed 3) (safety 1))
           (type fixnum x y w h)
           (type pixel-argb color))
  ...)
```

### D. Exhaustive Matching with `ecase` / `etypecase`
Avoid catch-all `case` clauses when handling typed domain variants. Use `ecase` or `etypecase` so SBCL signals exhaustive coverage errors or warnings if a case is missing:
```lisp
(ecase (window-split-type win)
  (:vertical   (layout-vertical win))
  (:horizontal (layout-horizontal win)))
```

### E. Defensive Runtime Boundaries
Use `check-type` at API boundaries accepting untyped external or user input:
```lisp
(check-type target-path (or string pathname))
```

---

## 6. Remote Access Over SSH

When working remotely over SSH, tunnel both VNC (port 5900) and SLY (port 4005):
```bash
ssh -L 5900:127.0.0.1:5900 -L 4005:127.0.0.1:4005 <user>@<remote-ip>
```
- **Live GUI:** Connect any VNC viewer (`vncviewer localhost:5900`, Remmina, TigerVNC, macOS Screen Sharing `vnc://localhost:5900`).
- **Live REPL:** In local Emacs, run `M-x sly-connect RET localhost RET 4005`.

