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
;;; randr / resolution / res - Display Resolution & Modesetting
;;; ---------------------------------------------------------------------------

(define-shell-command randr (engine &rest args)
  "Query or change display resolution.
Usage:
  randr                 List supported display modes and current resolution
  randr <w> <h>         Set resolution (e.g. randr 1920 1080)
  randr <w>x<h>         Set resolution (e.g. randr 1920x1080)
  randr auto            Switch to monitor's preferred/native resolution"
  (declare (ignore engine))
  (cond
    ;; No args: List supported modes
    ((null args)
     (let* ((modes (lispos.drm:list-modes))
            (cur-drm lispos.drm:*drm-state*)
            (cur-w (if cur-drm (lispos.drm:drm-state-width cur-drm) 0))
            (cur-h (if cur-drm (lispos.drm:drm-state-height cur-drm) 0)))
       (if (null modes)
           (format t "No display modes found or DRM uninitialized.~%")
           (progn
             (format t "Screen 0: current ~Ax~A~%" cur-w cur-h)
             (format t "  IDX  RESOLUTION    REFRESH  FLAGS~%")
             (format t "  ---------------------------------~%")
             (dolist (m modes)
               (let ((pref (if (lispos.drm:drm-mode-info-preferred-p m) "+" " "))
                     (curr (if (lispos.drm:drm-mode-info-current-p m) "*" " "))
                     (res-str (format nil "~Ax~A"
                                      (lispos.drm:drm-mode-info-width m)
                                      (lispos.drm:drm-mode-info-height m))))
                 (format t "  ~3D  ~9A  ~3DHz    ~A~A~%"
                         (lispos.drm:drm-mode-info-index m)
                         res-str
                         (lispos.drm:drm-mode-info-vrefresh m)
                         pref curr)))
             (format t "  (+ preferred, * current)~%")))))

    ;; Single arg: 'auto', 'preferred', or '<w>x<h>'
    ((= (length args) 1)
     (let ((arg (first args)))
       (cond
         ((or (string-equal arg "auto") (string-equal arg "preferred"))
          (let ((pref (lispos.drm:find-mode :preferred)))
            (if pref
                (handler-case
                    (let ((st (lispos.drm:set-mode (lispos.drm:drm-mode-info-width pref)
                                                  (lispos.drm:drm-mode-info-height pref))))
                      (format t "Resolution switched to native preferred mode ~Ax~A.~%"
                              (lispos.drm:drm-state-width st)
                              (lispos.drm:drm-state-height st)))
                  (error (c)
                    (format *error-output* "randr: error setting resolution: ~A~%" c)))
                (format *error-output* "randr: no preferred mode found.~%"))))
         (t
          (multiple-value-bind (w h) (lispos.drm:parse-resolution-string arg)
            (if (and w h)
                (handler-case
                    (progn
                      (lispos.drm:set-mode w h)
                      (format t "Resolution switched to ~Ax~A.~%" w h))
                  (error (c)
                    (format *error-output* "randr: error setting resolution: ~A~%" c)))
                (format *error-output* "randr: invalid resolution '~A'. Expected <w>x<h> or auto.~%" arg)))))))

    ;; Two args: '<w> <h>'
    ((= (length args) 2)
     (let ((w (parse-integer (first args) :junk-allowed t))
           (h (parse-integer (second args) :junk-allowed t)))
       (if (and w h (> w 0) (> h 0))
           (handler-case
               (progn
                 (lispos.drm:set-mode w h)
                 (format t "Resolution switched to ~Ax~A.~%" w h))
             (error (c)
               (format *error-output* "randr: error setting resolution: ~A~%" c)))
           (format *error-output* "randr: invalid dimensions '~A ~A'.~%" (first args) (second args)))))

    (t
     (format *error-output* "Usage: randr [<width> <height> | <width>x<height> | auto]~%")))
  nil)

(define-shell-command resolution (engine &rest args)
  "Alias for randr."
  (apply (gethash "randr" *shell-commands*) engine args))

(define-shell-command res (engine &rest args)
  "Alias for randr."
  (apply (gethash "randr" *shell-commands*) engine args))

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
  (format t "   edit [file]          Open file or *scratch* in Helix editor~%")
  (format t "   scratch              Open or switch to *scratch* editor buffer~%")
  (format t "   cat <file...>        Display file contents~%")
  (format t "   echo [-n] <args...>  Print arguments~%")
  (format t "   touch <file...>      Create empty file or update timestamp~%")
  (format t "   mkdir <dir...>       Create directories~%")
  (format t "   rm [-r] <path...>    Remove file or directory~%")
  (format t "   ps                   List active threads and processes~%")
  (format t "   whoami               Print current user name~%")
  (format t "   history              Display command history~%")
  (format t "   clear                Clear screen (C-l)~%")
  (format t "   randr [w h|auto]     Query or change display resolution (aliases: res, resolution)~%")
  (format t "   sync                 Flush filesystem buffers to storage~%")
  (format t "~%")
  (format t " [Common Lisp Mode] (surround expression with parentheses):~%")
  (format t "   (+ 1 2)              Evaluate any Common Lisp expression~%")
  (format t "   (defun foo () ...)   Define functions live in userland~%")
  (format t "   (directory #P\"/*\")   Call system Lisp APIs directly~%")
  (format t "=====================================================================~%~%")
  nil)
