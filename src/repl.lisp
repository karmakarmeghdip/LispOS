;;;; src/repl.lisp - Serial Console REPL and Slynk/Swank Server for LispOS

(in-package :lispos.repl)

(defvar *repl-thread* nil)

(defun start-slynk-server (&optional (port 4005) (interface "0.0.0.0"))
  "Start Slynk remote development server on specified port."
  (format t "[REPL] Starting Slynk remote server on ~A:~A...~%" interface port)
  (handler-case
      (progn
        (slynk:create-server :port port :interface interface :dont-close t)
        (format t "[REPL] Slynk server active on port ~A.~%" port))
    (error (c)
      (format t "[REPL] Warning: Failed to start Slynk server: ~A~%" c))))

(defun run-repl-loop ()
  "Standard read-eval-print loop with interactive restarts."
  (setf *default-pathname-defaults* (pathname lispos.paths:+admin-home+))
  (setf *package* (find-package :cl-user))
  (format t "~%======================================================~%")
  (format t " Welcome to LispOS (PID 1 Common Lisp Monolithic Userland)~%")
  (format t " Type any Common Lisp expression to evaluate.~%")
  (format t " Try: (directory #P\"/*\") or (lispos.drm:draw-rect 100 100 200 100 #xFFFF5500)~%")
  (format t " SLY/Swank listening on port 4005.~%")
  (format t "======================================================~%~%")
  (loop
    (restart-case
        (handler-bind
            ((error (lambda (c)
                      (format *error-output* "~&[Debugger] Unhandled error: ~A~%" c)
                      (invoke-debugger c))))
          (format t "~A> " (package-name *package*))
          (finish-output)
          (let ((form (read *standard-input* nil :eof)))
            (if (eq form :eof)
                (return)
                (let ((results (multiple-value-list (eval form))))
                  (dolist (val results)
                    (format t "~S~%" val))
                  (finish-output)))))
      (abort ()
        :report "Return to LispOS top-level REPL."
        (format t "~&[REPL] Aborted back to top-level.~%"))
      (retry ()
        :report "Retry last expression."
        nil))))

(defun open-serial-console ()
  "Attempt to open serial console device /System/Kernel/Devices/ttyS0."
  (let ((dev-path (if (probe-file lispos.paths:+serial-console+)
                      lispos.paths:+serial-console+
                      (if (probe-file "/dev/ttyS0")
                          "/dev/ttyS0"
                          nil))))
    (when dev-path
      (ignore-errors
       (let ((fd (sb-posix:open dev-path (logior sb-posix:o-rdwr sb-posix:o-noctty))))
         (when (and fd (>= fd 0))
           (sb-sys:make-fd-stream fd :input t :output t :buffering :none :element-type :default)))))))

(defun start-serial-repl ()
  "Launch dedicated REPL thread on serial console or stdio."
  (let ((console-stream (open-serial-console)))
    (setf *repl-thread*
          (sb-thread:make-thread
           (lambda ()
             (if console-stream
                 (let ((*standard-input* console-stream)
                       (*standard-output* console-stream)
                       (*error-output* console-stream)
                       (*terminal-io* console-stream))
                   (run-repl-loop))
                 (run-repl-loop)))
           :name "LispOS-REPL-Worker"))))
