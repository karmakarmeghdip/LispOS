;;;; src/init.lisp - PID 1 Boot Sequence and Immortal Supervisor for LispOS

(in-package :lispos.init)

(defun mount-filesystems ()
  "Mount Linux kernel pseudo-filesystems under /System/Kernel/* and runtime tmpfs."
  (format t "[INIT] Setting up non-FHS filesystem hierarchy...~%")
  (lispos.paths:ensure-system-hierarchy)

  ;; Mount devtmpfs directly at /System/Kernel/Devices
  (format t "[INIT] Mounting devtmpfs at ~A...~%" lispos.paths:+dev-root+)
  (unless (lispos.sys:mount "devtmpfs" lispos.paths:+dev-root+ "devtmpfs")
    (format t "[INIT] Warning: devtmpfs mount failed (errno ~A)~%" (sb-alien:get-errno)))

  ;; Clean up kernel-created /dev and /root stubs so / has only non-FHS folders
  (ignore-errors (lispos.sys:umount "/dev" lispos.sys:+mnt-detach+))
  (ignore-errors (sb-posix:rmdir "/dev"))
  (ignore-errors (sb-posix:rmdir "/root"))

  ;; Mount procfs
  (format t "[INIT] Mounting procfs at ~A...~%" lispos.paths:+proc-root+)
  (unless (lispos.sys:mount "proc" lispos.paths:+proc-root+ "proc")
    (format t "[INIT] Warning: proc mount failed (errno ~A)~%" (sb-alien:get-errno)))

  ;; Mount sysfs
  (format t "[INIT] Mounting sysfs at ~A...~%" lispos.paths:+sys-root+)
  (unless (lispos.sys:mount "sysfs" lispos.paths:+sys-root+ "sysfs")
    (format t "[INIT] Warning: sysfs mount failed (errno ~A)~%" (sb-alien:get-errno)))

  ;; Mount ephemeral tmpfs for runtime sockets/state
  (format t "[INIT] Mounting tmpfs at ~A...~%" lispos.paths:+system-runtime+)
  (unless (lispos.sys:mount "tmpfs" lispos.paths:+system-runtime+ "tmpfs")
    (format t "[INIT] Warning: runtime tmpfs mount failed (errno ~A)~%" (sb-alien:get-errno))))

(defun draw-boot-splash ()
  "Render a clean boot banner and status onto the KMS framebuffer."
  (when lispos.drm:*drm-state*
    (let ((w (lispos.drm:drm-state-width lispos.drm:*drm-state*))
          (h (lispos.drm:drm-state-height lispos.drm:*drm-state*)))
      ;; Background
      (lispos.drm:clear-screen #xFF1E1E2E)
      ;; Header bar
      (lispos.drm:draw-rect 0 0 w 48 #xFF313244)
      (lispos.drm:draw-string 20 16 "LispOS  --  Monolithic SBCL Userland" #xFFCDD6F4 nil 2)
      ;; Central card
      (let ((cx 40)
            (cy 80))
        (lispos.drm:draw-rect cx cy (- w 80) (- h 120) #xFF181825)
        (lispos.drm:draw-string (+ cx 20) (+ cy 20) "System Status: ONLINE" #xFFA6E3A1 nil 2)
        (lispos.drm:draw-string (+ cx 20) (+ cy 60) "PID 1: Monolithic SBCL Common Lisp" #xFF89B4FA nil 2)
        (lispos.drm:draw-string (+ cx 20) (+ cy 100) "Hierarchy: /System  /Applications  /Library  /Users" #xFFF9E2AF nil 2)
        (lispos.drm:draw-string (+ cx 20) (+ cy 140) "SLY / Swank Remote Server: port 4005" #xFFF38BA8 nil 2)
        (lispos.drm:draw-string (+ cx 20) (+ cy 180) "TTY REPL: active on serial console (ttyS0)" #xFFBAC2DE nil 2)
        (lispos.drm:draw-string (+ cx 20) (+ cy 240) "Evaluate (lispos.drm:draw-rect ...) in REPL to draw!" #xFFCBA6F7 nil 2)))))

(defun boot-os ()
  "Top-level PID 1 entry point for LispOS."
  (format t "~%======================================================~%")
  (format t " >>> Booting LispOS (Common Lisp Monolithic Userland) <<<~%")
  (format t "======================================================~%~%")

  ;; 1. Configure Logical Pathnames
  (lispos.paths:setup-logical-pathnames)

  ;; 2. Install Zombie Process Reaper
  (lispos.sys:install-zombie-reaper)

  ;; 3. Mount Pseudo-Filesystems (Non-FHS)
  (ignore-errors (mount-filesystems))

  ;; 4. Bring up Networking
  (format t "[INIT] Initializing network interfaces...~%")
  (ignore-errors
   (lispos.sys:setup-network-interface "lo" "127.0.0.1" :is-loopback t))
  (ignore-errors
   (lispos.sys:setup-network-interface "eth0" "10.0.2.15" :netmask-str "255.255.255.0"))

  ;; 5. Start SLY/Swank Server
  (ignore-errors
   (lispos.repl:start-slynk-server 4005 "0.0.0.0"))

  ;; 6. Initialize DRM/KMS Framebuffer
  (format t "[INIT] Initializing KMS Graphics Display...~%")
  (handler-case
      (progn
        (lispos.drm:init-drm)
        (draw-boot-splash))
    (error (c)
      (format t "[INIT] DRM initialization note: ~A~%" c)))

  ;; 7. Start evdev Input Subsystem
  (format t "[INIT] Starting evdev input listener...~%")
  (ignore-errors
   (lispos.input:start-input-loop))

  ;; 8. Start Serial Console REPL
  (format t "[INIT] Starting TTY REPL...~%")
  (ignore-errors
   (lispos.repl:start-serial-repl))

  ;; 9. Immortal PID 1 Supervisor Loop (Crash Immunity)
  (format t "[INIT] PID 1 Immortal Supervisor Loop Active.~%~%")
  (loop
    (restart-case
        (handler-bind
            ((error (lambda (c)
                      (format t "[SUPERVISOR] Caught condition in PID 1: ~A~%" c)
                      (invoke-restart 'recover))))
          ;; Heartbeat and periodic maintenance
          (loop
            (sleep 2)
            (lispos.sys:reap-zombies)))
      (recover ()
        :report "Recover LispOS supervisor loop from crash."
        (format t "[SUPERVISOR] Recovered PID 1 loop.~%")))))
