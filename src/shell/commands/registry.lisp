;;;; src/shell/commands/registry.lisp - Command Registry and Definition Macro

(in-package :lispos.shell)

(defvar *shell-commands* (make-hash-table :test #'equalp)
  "Registry mapping shell command names to handler functions.")

(defmacro define-shell-command (name (engine &rest args) &body body)
  "Register a shell command NAME. The handler receives ENGINE and ARGS (list of strings)."
  (let ((cmd-str (string-downcase (string name))))
    `(progn
       (setf (gethash ,cmd-str *shell-commands*)
             (lambda (,engine ,@args)
               ,@body))
       ',name)))

(declaim (ftype (function ((or string symbol)) (or function null)) get-shell-command))
(defun get-shell-command (cmd-name)
  "Look up shell command handler for CMD-NAME."
  (gethash (string-downcase (string cmd-name)) *shell-commands*))
