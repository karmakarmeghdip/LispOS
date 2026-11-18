;;;; src/hal/input/package.lisp - Package Definition for Input Subsystem

(defpackage :lispos.input
  (:use :cl)
  (:export
   #:start-input-loop
   #:stop-input-loop
   #:register-key-hook
   #:register-pointer-hook
   #:poll-input-event
   #:enqueue-input-event
   #:scancode->keysym
   #:input-event
   #:make-input-event
   #:input-event-type
   #:input-event-key
   #:input-event-char
   #:input-event-modifiers
   #:modifier-active-p))
