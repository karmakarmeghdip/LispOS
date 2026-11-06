# LispOS: Monolithic Common Lisp Linux Userland

A minimal, self-contained Linux operating system userland written entirely in Common Lisp (SBCL) that boots directly as `/System/Core/Init` (PID 1) inside a Linux initramfs over a standard Linux kernel (`vmlinuz`).

## Key Architectural Principles

- **No GNU Userland & No Legacy FHS**: Eliminates `/bin`, `/usr`, `/etc`, `/var`, `/lib64`, and `/sbin`. Employs a clean, human-readable directory hierarchy inspired by macOS and Lisp Machines.
- **Monolithic Single-Image Userland**: PID 1 is a single compiled SBCL image (`save-lisp-and-die :executable t`) containing the init supervisor, DRM/KMS dumb-buffer framebuffer driver, evdev input loop, live serial console REPL, and remote SLY/Swank server.
- **Crash Immunity (PID 1 Guard)**: Main supervisor loop and worker threads are wrapped in `handler-bind` / `restart-case` blocks so unhandled conditions never cause PID 1 to exit (avoiding kernel panics).
- **Source Introspection**: Logical pathnames (`SYS:SRC;**;*.*.*` and `LISPOS:**;*.*.*`) mapped to `/System/Source/` so `M-.` (`xref-find-definitions`) resolves down to the lowest level of the OS.

---

## Directory Hierarchy (Non-FHS Root)

```
/
├── System/
│   ├── Core/
│   │   └── Init                  # Monolithic SBCL PID 1 executable image
│   ├── Libraries/                # Shared C libraries (.so) & ELF dynamic linker
│   ├── Source/
│   │   ├── LispOS/               # Live OS Lisp source files (.lisp)
│   │   └── SBCL/                 # SBCL compiler/runtime source for M-. introspection
│   ├── Configuration/            # System-wide declarative S-expression configs (.sexp)
│   ├── Runtime/                  # Ephemeral runtime sockets & state (mounted tmpfs)
│   └── Kernel/                   # Kernel pseudo-filesystems (hidden away from /)
│       ├── Processes/            # procfs mount (replaces /proc)
│       ├── Hardware/             # sysfs mount (replaces /sys)
│       └── Devices/              # devtmpfs mount (replaces /dev)
├── Applications/                 # Self-contained application bundles
├── Library/                      # Shared packages, fonts, and themes
│   └── Fonts/
└── Users/
    └── Admin/                    # Default user home directory
```

---

## Host Prerequisites & Setup

### Arch Linux / CachyOS Packages

To run QEMU with display output:
```bash
sudo pacman -S qemu-desktop
```
*(Or `sudo pacman -S qemu-system-x86 qemu-ui-gtk qemu-ui-sdl`)*

Installed host tools used for building:
- `sbcl` (with core compression)
- `patchelf`
- `cpio`
- `gzip`
- `make`

---

## Building and Running

### 1. Build the Monolithic SBCL Executable Image
```bash
make image
```
Produces `build/Init` (~13MB compressed).

### 2. Pack the Non-FHS initramfs
```bash
make initramfs
```
Creates the `/System`, `/Applications`, `/Library`, `/Users` structure, copies dependencies into `/System/Libraries/`, runs `patchelf` on `Init` to request `/System/Libraries/ld-linux-x86-64.so.2` with RPATH `/System/Libraries`, and packs `build/initramfs.cpio.gz` (~15MB).

### 3. Launch with QEMU

- **Terminal stdio mode (No GUI required)**:
  ```bash
  make run-stdio
  ```
  Runs headlessly directly in your current terminal. Exit anytime with `(lispos.sys:poweroff)` or `Ctrl-a x`.

- **Graphical window mode**:
  ```bash
  make run
  ```
  Boots with KMS display output on the host window while keeping the serial REPL connected to stdio.

---

## Live REPL & SLY Development

1. **Serial Console REPL**:
   Upon boot, QEMU's terminal provides a live Common Lisp REPL:
   ```lisp
   COMMON-LISP-USER> (directory #P"/*")
   (#P"/Applications/" #P"/Library/" #P"/System/" #P"/Users/")

   COMMON-LISP-USER> (lispos.drm:draw-rect 100 100 300 200 #xFFFF5500)
   ```
2. **SLY Remote Connection (Port 4005)**:
   In Emacs on the host:
   `M-x sly-connect RET 127.0.0.1 RET 4005`
   Recompile functions, update graphics, or inspect threads live without restarting the VM!

---

## Development & Verification Harness

A complete CLI harness is provided via `tools/harness.py` and `Makefile` for headless daemon execution, remote evaluation, and display capture:

| Command | Description |
|---|---|
| `make start` | Launch LispOS as background QEMU daemon (auto-builds initramfs if needed) |
| `make status` | Check PID, QMP socket, SLY port, and serial log status |
| `make eval EXPR='...'` | Safely evaluate any Common Lisp expression over SLY/Slynk (port 4005) |
| `make screenshot [OUTPUT=path.png]` | Auto-flush framebuffer & capture display to PNG via QMP screendump |
| `make repl` | Interactive CLI REPL connected directly to running LispOS |
| `make log` | View the live serial console log (`build/serial.log`) |
| `make stop` | Gracefully shut down the background QEMU daemon |

### Example Workflow
```bash
# 1. Start daemon
make start

# 2. Draw a window live on the running framebuffer
./tools/harness.py eval '(progn
  (lispos.drm:draw-rect 300 200 680 360 #xFF24273A)
  (lispos.drm:draw-rect 300 200 680 40 #xFF8AADF4)
  (lispos.drm:draw-string 320 212 "LispOS Interactive Window" #xFF181926 nil 2)
  (lispos.drm:flush-screen))'

# 3. Capture screen to PNG
make screenshot

# 4. Stop when finished
make stop
```

