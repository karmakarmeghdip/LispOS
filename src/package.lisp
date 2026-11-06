;;;; src/package.lisp - Package Definitions for LispOS

(defpackage :lispos.paths
  (:use :cl)
  (:export
   #:+root+
   #:+system-root+
   #:+system-core+
   #:+system-libraries+
   #:+system-source+
   #:+system-configuration+
   #:+system-runtime+
   #:+proc-root+
   #:+sys-root+
   #:+dev-root+
   #:+drm-card0+
   #:+serial-console+
   #:+applications-root+
   #:+library-root+
   #:+users-root+
   #:+admin-home+
   #:setup-logical-pathnames
   #:ensure-system-hierarchy))

(defpackage :lispos.sys
  (:use :cl)
  (:export
   #:mount
   #:umount
   #:ioctl
   #:setup-network-interface
   #:install-zombie-reaper
   #:reap-zombies
   #:sync-filesystem
   #:poweroff
   #:reboot
   #:load-kernel-module
   #:+ms-rdonly+
   #:+ms-nosuid+
   #:+ms-nodev+
   #:+ms-noexec+
   #:+ms-bind+
   #:+ms-move+
   #:+mnt-detach+))

(defpackage :lispos.drm
  (:use :cl)
  (:export
   #:*drm-state*
   #:drm-state
   #:drm-state-fd
   #:drm-state-crtc-id
   #:drm-state-connector-id
   #:drm-state-fb-id
   #:drm-state-handle
   #:drm-state-width
   #:drm-state-height
   #:drm-state-pitch
   #:drm-state-size
   #:drm-state-sap
   #:init-drm
   #:close-drm
   #:clear-screen
   #:draw-rect
   #:draw-string
   #:draw-char
   #:rgb
   #:argb))

(defpackage :lispos.input
  (:use :cl)
  (:export
   #:start-input-loop
   #:stop-input-loop
   #:register-key-hook
   #:register-pointer-hook))

(defpackage :lispos.repl
  (:use :cl)
  (:export
   #:start-serial-repl
   #:start-slynk-server))

(defpackage :lispos.sandbox
  (:use :cl)
  (:export
   #:safe-eval
   #:make-sandbox-env))

(defpackage :lispos.init
  (:use :cl)
  (:export
   #:boot-os))
