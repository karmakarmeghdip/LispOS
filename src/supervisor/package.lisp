;;;; src/supervisor/package.lisp - Package Definitions for Supervisor Subsystem

(defpackage :lispos.sandbox
  (:use :cl)
  (:export
   #:safe-eval
   #:make-sandbox-env))

(defpackage :lispos.repl
  (:use :cl :lispos.shell)
  (:export
   #:start-serial-repl
   #:start-slynk-server
   ;; Re-export shell engine APIs from lispos.shell
   #:*default-prompt*
   #:*prompt-fn*
   #:default-lispos-prompt
   #:get-cwd-namestring
   #:*shell-commands*
   #:define-shell-command
   #:get-shell-command
   #:tokenize-shell-line
   #:resolve-path
   #:evaluate-shell-line
   #:shell-engine
   #:shell-engine-p
   #:make-shell-engine
   #:engine-buffer
   #:engine-point
   #:engine-prompt
   #:engine-history
   #:engine-output-history
   #:engine-last-eval-values
   #:engine-last-stdout
   #:engine-last-stderr
   #:engine-last-error
   #:engine-last-status
   #:buffer-text
   #:buffer-point
   #:set-buffer-text
   #:set-buffer-point
   #:handle-key
   #:handle-char
   #:feed-input
   #:balanced-parentheses-p
   #:submit-buffer
   #:evaluate-string
   #:output-history-lines
   #:render-prompt-and-buffer
   #:run-shell-tests))

(defpackage :lispos.init
  (:use :cl)
  (:export
   #:boot-os))
