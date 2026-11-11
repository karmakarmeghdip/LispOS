;;;; src/commands/system.lisp - Built-in System Commands for LispOS Shell
;;;; Implements echo, ps, whoami, clear, history, sync, help.

(in-package :lispos.shell)

;;; ---------------------------------------------------------------------------
;;; echo - Print Arguments
;;; ---------------------------------------------------------------------------

(define-shell-command echo (engine &rest args)
  "Print arguments separated by spaces (-n suppresses trailing newline)."
  (declare (ignore engine))
  (let ((no-newline nil)
        (tokens args))
    (when (and tokens (string= (first tokens) "-n"))
      (setf no-newline t)
      (setf tokens (rest tokens)))
    (format t "~{~A~^ ~}" tokens)
    (unless no-newline
      (terpri)))
  nil)

;;; ---------------------------------------------------------------------------
;;; ps - List Active Threads / Processes
;;; ---------------------------------------------------------------------------

(define-shell-command ps (engine &rest args)
  "List active SBCL threads and system processes."
  (declare (ignore engine args))
  (format t "  TID  STATUS   NAME~%")
  (format t "--------------------------------------------------------~%")
  (dolist (th (sb-thread:list-all-threads))
    (let ((tid (or (ignore-errors (sb-thread::thread-os-tid th)) 0))
          (status (if (sb-thread:thread-alive-p th) "RUNNING" "STOPPED"))
          (name (or (sb-thread:thread-name th) "anonymous")))
      (format t "  ~4D  ~7A  ~A~%" tid status name)))
  nil)

;;; ---------------------------------------------------------------------------
;;; whoami - Display Current User
;;; ---------------------------------------------------------------------------

(define-shell-command whoami (engine &rest args)
  "Display current operating system user identity."
  (declare (ignore engine args))
  (format t "Admin~%")
  nil)

;;; ---------------------------------------------------------------------------
;;; clear - Clear Shell Output Buffer
;;; ---------------------------------------------------------------------------

(define-shell-command clear (engine &rest args)
  "Clear the interactive shell output history."
  (declare (ignore args))
  (setf (shell-engine-output-history engine) '())
  nil)

;;; ---------------------------------------------------------------------------
;;; history - Display Command History
;;; ---------------------------------------------------------------------------

(define-shell-command history (engine &rest args)
  "Display numbered list of commands executed in the current session."
  (declare (ignore args))
  (let ((hist (reverse (shell-engine-history engine))))
    (if (null hist)
        (format t "No commands in history.~%")
        (loop for cmd in hist
              for idx from 1 do
                (format t "  ~3D  ~A~%" idx cmd))))
  nil)

;;; ---------------------------------------------------------------------------
;;; sync - Flush Filesystem Caches
;;; ---------------------------------------------------------------------------

(define-shell-command sync (engine &rest args)
  "Synchronize dirty filesystem blocks to disk."
  (declare (ignore engine args))
  (handler-case
      (progn
        (lispos.sys:sync-filesystem)
        (format t "Filesystems synced successfully.~%"))
    (error (c)
      (format *error-output* "sync failed: ~A~%" c)))
  nil)

;;; ---------------------------------------------------------------------------
;;; help - Interactive Command Guide
;;; ---------------------------------------------------------------------------

(define-shell-command help (engine &rest args)
  "Display reference guide for LispOS shell and Common Lisp REPL."
  (declare (ignore engine args))
  (format t "~%=================== LispOS Interactive Shell Help ===================~%")
  (format t " Dual-Mode Shell: Unix-like commands and Common Lisp REPL unified.~%~%")
  (format t " [Command Mode] (no surrounding parentheses):~%")
  (format t "   ls [-l] [-a] [path]  List directory contents~%")
  (format t "   cd [path]            Change working directory (updates prompt)~%")
  (format t "   pwd                  Print current working directory~%")
  (format t "   cat <file...>        Display file contents~%")
  (format t "   echo [-n] <args...>  Print arguments~%")
  (format t "   touch <file...>      Create empty file or update timestamp~%")
  (format t "   mkdir <dir...>       Create directories~%")
  (format t "   rm [-r] <path...>    Remove file or directory~%")
  (format t "   ps                   List active threads and processes~%")
  (format t "   whoami               Print current user name~%")
  (format t "   history              Display command history~%")
  (format t "   clear                Clear screen (C-l)~%")
  (format t "   sync                 Flush filesystem buffers to storage~%")
  (format t "~%")
  (format t " [Common Lisp Mode] (surround expression with parentheses):~%")
  (format t "   (+ 1 2)              Evaluate any Common Lisp expression~%")
  (format t "   (defun foo () ...)   Define functions live in userland~%")
  (format t "   (directory #P\"/*\")   Call system Lisp APIs directly~%")
  (format t "=====================================================================~%~%")
  nil)
