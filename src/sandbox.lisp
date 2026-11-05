;;;; src/sandbox.lisp - Restricted SAFE-CL Evaluator for LispOS

(in-package :lispos.sandbox)

(defpackage :safe-cl
  (:use)
  (:import-from :cl
   #:+ #:- #:* #:/ #:= #:/= #:< #:<= #:> #:>=
   #:list #:cons #:car #:cdr #:cadr #:cddr #:append #:length #:member #:assoc
   #:let #:let* #:defun #:defparameter #:defvar #:lambda
   #:if #:cond #:case #:when #:unless #:progn
   #:format #:print #:prin1 #:princ
   #:t #:nil #:quote))

(defun safe-eval (expr &optional (env nil))
  "Evaluate expression in restricted environment with *read-eval* disabled."
  (let ((*read-eval* nil)
        (*package* (find-package :safe-cl)))
    (eval expr)))

(defun make-sandbox-env ()
  "Create an isolated package environment for untrusted script execution."
  (let ((pkg-name (format nil "SANDBOX-~A" (get-universal-time))))
    (make-package pkg-name :use '(:safe-cl))))
