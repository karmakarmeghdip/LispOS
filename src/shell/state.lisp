;;;; src/shell/state.lisp - Shell Engine State and Configuration

(in-package :lispos.shell)

(defparameter +default-prompt+ "λ > "
  "Standard prompt prefix for the LispOS shell.")

(defvar *prompt-fn* nil
  "Custom prompt hook function (lambda (engine) ...).")

(defvar *default-prompt* :dynamic
  "Configurable default prompt string, :dynamic, or prompt function.")

(declaim (ftype (function () string) get-cwd-namestring))
(defun get-cwd-namestring ()
  "Return current working directory as a clean pathname string."
  (let ((raw (or (ignore-errors (namestring (truename *default-pathname-defaults*)))
                 (ignore-errors (namestring *default-pathname-defaults*))
                 "/")))
    (if (and (> (length raw) 1) (char= (char raw (1- (length raw))) #\/))
        (subseq raw 0 (1- (length raw)))
        raw)))

(declaim (ftype (function (&optional t) string) default-lispos-prompt))
(defun default-lispos-prompt (&optional engine)
  "Render standard LispOS prompt showing CWD abbreviated with ~ for /Users/Admin."
  (declare (ignore engine))
  (let* ((cwd (get-cwd-namestring))
         (admin-home (ignore-errors (namestring (pathname lispos.paths:+admin-home+))))
         (clean-home (if (and admin-home (> (length admin-home) 1)
                              (char= (char admin-home (1- (length admin-home))) #\/))
                          (subseq admin-home 0 (1- (length admin-home)))
                          (or admin-home "/Users/Admin")))
         (display-cwd (cond
                        ((string= cwd clean-home) "~")
                        ((and clean-home (search clean-home cwd) (= (search clean-home cwd) 0))
                         (concatenate 'string "~" (subseq cwd (length clean-home))))
                        (t cwd))))
    (format nil "λ [~A] > " display-cwd)))

(defstruct (shell-engine (:constructor %make-shell-engine))
  "State for the interactive REPL shell engine."
  (prompt :dynamic)
  (buffer (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)
   :type (vector character))
  (point 0 :type fixnum)
  (history '() :type list)
  (history-max 500 :type fixnum)
  (history-index nil :type (or null fixnum)) ; nil when not browsing history, else 0-indexed integer
  (scratch-buffer "" :type string)
  (scratch-point 0 :type fixnum)
  (output-history '() :type list) ; stored most-recent first
  (package (find-package :cl-user) :type package)
  (last-eval-values '() :type list)
  (last-stdout "" :type string)
  (last-stderr "" :type string)
  (last-error nil)
  (last-status nil :type symbol)) ; :submitted, :incomplete, :empty, :error

(defun make-shell-engine (&key (prompt *default-prompt*)
                               (package (find-package :cl-user))
                               (history-max 500))
  "Create and initialize a new REPL shell engine instance."
  (%make-shell-engine
   :prompt prompt
   :buffer (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)
   :point 0
   :history '()
   :history-max history-max
   :history-index nil
   :scratch-buffer ""
   :scratch-point 0
   :output-history '()
   :package (if (packagep package) package (find-package package))
   :last-eval-values '()
   :last-stdout ""
   :last-stderr ""
   :last-error nil
   :last-status nil))

(defun engine-buffer (engine)
  "Return current buffer content as a string."
  (copy-seq (shell-engine-buffer engine)))

(defun engine-point (engine)
  "Return cursor position point."
  (shell-engine-point engine))

(defun engine-prompt (engine)
  "Return resolved prompt string for ENGINE."
  (let ((p (shell-engine-prompt engine)))
    (cond
      ((functionp p) (funcall p engine))
      ((eq p :dynamic)
       (if *prompt-fn*
           (funcall *prompt-fn* engine)
           (default-lispos-prompt engine)))
      ((stringp p) p)
      (*prompt-fn* (funcall *prompt-fn* engine))
      (t (default-lispos-prompt engine)))))

(defun (setf engine-prompt) (val engine)
  (setf (shell-engine-prompt engine) val))

(defun engine-history (engine)
  (shell-engine-history engine))

(defun engine-history-index (engine)
  (shell-engine-history-index engine))

(defun engine-scratch-buffer (engine)
  (shell-engine-scratch-buffer engine))

(defun engine-scratch-point (engine)
  (shell-engine-scratch-point engine))

(defun engine-output-history (engine)
  (shell-engine-output-history engine))

(defun engine-package (engine)
  (shell-engine-package engine))

(defun engine-last-eval-values (engine)
  (shell-engine-last-eval-values engine))

(defun engine-last-stdout (engine)
  (shell-engine-last-stdout engine))

(defun engine-last-stderr (engine)
  (shell-engine-last-stderr engine))

(defun engine-last-error (engine)
  (shell-engine-last-error engine))

(defun engine-last-status (engine)
  (shell-engine-last-status engine))
