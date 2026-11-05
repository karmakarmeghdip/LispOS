;;;; build-image.lisp - Compile and dump LispOS monolithic SBCL PID 1 executable

(in-package :cl-user)

(format t "==> Setting up Quicklisp and ASDF paths...~%")
(let ((quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-setup)
    (load quicklisp-setup)))

(pushnew (truename ".") asdf:*central-registry* :test #'equal)

(format t "==> Loading LispOS system and dependencies...~%")
(ql:quickload :lispos)

(format t "==> Configuring CFFI foreign library search paths...~%")
(pushnew #P"/System/Libraries/" cffi:*foreign-library-directories* :test #'equal)

(format t "==> Dumping monolithic executable build/Init...~%")
(ensure-directories-exist "build/")

(sb-ext:save-lisp-and-die
 "build/Init"
 :toplevel #'lispos.init:boot-os
 :executable t
 :compression t)
