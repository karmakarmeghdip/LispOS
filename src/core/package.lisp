;;;; src/core/package.lisp - Package Definitions for Core Subsystem

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
