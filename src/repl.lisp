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
  (ignore-errors (sb-posix:chdir (namestring *default-pathname-defaults*)))
  (setf *package* (find-package :cl-user))
  (format t "~%======================================================~%")
  (format t " Welcome to LispOS (PID 1 Common Lisp Monolithic Userland)~%")
  (format t " Type any Common Lisp expression to evaluate.~%")
  (format t " Try: (directory #P\"/*\") or (lispos.drm:draw-rect 100 100 200 100 #xFFFF5500)~%")
  (format t " SLY/Swank listening on port 4005.~%")
  (format t "======================================================~%~%")
  (loop
    (restart-case
        (handler-case
            (progn
              (format t "~A> " (package-name *package*))
              (finish-output)
              (let ((form (read *standard-input* nil :eof)))
                (if (eq form :eof)
                    (return)
                    (let ((results (multiple-value-list (eval form))))
                      (dolist (val results)
                        (format t "~S~%" val))
                      (finish-output)))))
          (error (c)
            (format t "~&[REPL Error] ~A~%" c)
            (finish-output)))
      (abort ()
        :report "Return to LispOS top-level REPL."
        (format t "~&[REPL] Aborted back to top-level.~%"))
      (retry ()
        :report "Retry last expression."
        nil))))

(defun start-serial-repl ()
  "Launch dedicated REPL thread on stdio."
  (setf *repl-thread*
        (sb-thread:make-thread #'run-repl-loop :name "LispOS-REPL-Worker")))
