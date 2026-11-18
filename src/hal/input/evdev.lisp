;;;; src/hal/input/evdev.lisp - evdev Event Listener Loop and Queue

(in-package :lispos.input)

(defconstant +epoll-ctl-add+ 1)
(defconstant +epoll-ctl-del+ 2)
(defconstant +epoll-ctl-mod+ 3)
(defconstant +epollin+ #x001)

(defconstant +ev-syn+ 0)
(defconstant +ev-key+ 1)
(defconstant +ev-rel+ 2)
(defconstant +ev-abs+ 3)

(defconstant +rel-x+ 0)
(defconstant +rel-y+ 1)

(cffi:defcfun ("epoll_create1" %epoll-create1) :int
  (flags :int))

(cffi:defcfun ("epoll_ctl" %epoll-ctl) :int
  (epfd :int)
  (op :int)
  (fd :int)
  (event :pointer))

(cffi:defcfun ("epoll_wait" %epoll-wait) :int
  (epfd :int)
  (events :pointer)
  (maxevents :int)
  (timeout :int))

(cffi:defcfun ("read" %c-read) :long
  (fd :int)
  (buf :pointer)
  (count :size))

(deftype input-event-type ()
  '(member :key-down :key-up :pointer))

(defstruct (input-event
            (:constructor %make-input-event))
  (type :key-down :type input-event-type)
  (key nil :type (or symbol character null))
  (char nil :type (or character null))
  (modifiers nil :type list))

(defun make-input-event (&key (type :key-down) key char modifiers)
  (%make-input-event :type type
                     :key key
                     :char char
                     :modifiers modifiers))

(declaim (ftype (function (input-event symbol) boolean) modifier-active-p))
(defun modifier-active-p (event mod)
  "Check if modifier MOD is active on EVENT."
  (not (null (member mod (input-event-modifiers event)))))

;; State variables
(defvar *input-thread* nil)
(defvar *running-input-loop* nil)
(defvar *key-hooks* nil)
(defvar *pointer-hooks* nil)

(defvar *cursor-x* 100)
(defvar *cursor-y* 100)

;; Modifier states
(defvar *modifier-shift* nil)
(defvar *modifier-ctrl* nil)
(defvar *modifier-alt* nil)
(defvar *modifier-super* nil)

;; Synchronized event queue
(defvar *event-queue* '())
(defvar *event-lock* (sb-thread:make-mutex :name "LispOS-Input-Lock"))

(declaim (ftype (function (input-event) list) enqueue-input-event))
(defun enqueue-input-event (event)
  "Push event to the synchronized event queue."
  (sb-thread:with-mutex (*event-lock*)
    (setf *event-queue* (nconc *event-queue* (list event)))))

(declaim (ftype (function () (or input-event null)) poll-input-event))
(defun poll-input-event ()
  "Pop the next input-event from the queue, or nil if empty."
  (sb-thread:with-mutex (*event-lock*)
    (pop *event-queue*)))

(defun active-modifiers ()
  "Return list of active modifier symbols."
  (let ((mods '()))
    (when *modifier-super* (push :super mods))
    (when *modifier-ctrl* (push :ctrl mods))
    (when *modifier-alt* (push :alt mods))
    (when *modifier-shift* (push :shift mods))
    mods))

(defun register-key-hook (hook-fn)
  "Register a callback (lambda (code value)) for key events."
  (push hook-fn *key-hooks*))

(defun register-pointer-hook (hook-fn)
  "Register a callback (lambda (x y)) for pointer position changes."
  (push hook-fn *pointer-hooks*))

(defun find-input-event-devices ()
  "Locate evdev event devices under /System/Kernel/Devices/input/ or /dev/input/."
  (let ((candidates (or (directory "/System/Kernel/Devices/input/event*")
                        (directory "/dev/input/event*"))))
    (mapcar #'namestring candidates)))

(defun handle-input-event (type code value)
  "Internal dispatcher for raw evdev events."
  (cond
    ((= type +ev-rel+)
     (when (= code +rel-x+)
       (incf *cursor-x* value)
       (setf *cursor-x* (max 0 (min *cursor-x* (or (and lispos.drm:*drm-state* (lispos.drm:drm-state-width lispos.drm:*drm-state*)) 1024)))))
     (when (= code +rel-y+)
       (incf *cursor-y* value)
       (setf *cursor-y* (max 0 (min *cursor-y* (or (and lispos.drm:*drm-state* (lispos.drm:drm-state-height lispos.drm:*drm-state*)) 768)))))
     (dolist (fn *pointer-hooks*)
       (ignore-errors (funcall fn *cursor-x* *cursor-y*))))

    ((= type +ev-key+)
     ;; Track modifier keys (1 = pressed, 0 = released, 2 = autorepeat)
     (let ((pressed (or (= value 1) (= value 2))))
       (case code
         ((42 54) (setf *modifier-shift* pressed))
         ((29 97) (setf *modifier-ctrl* pressed))
         ((56 100) (setf *modifier-alt* pressed))
         ((125 126) (setf *modifier-super* pressed)))
       ;; Only emit key-down events to queue for consumption (skip emitting on modifier presses themselves)
       (when (and pressed (not (member code '(42 54 29 97 56 100 125 126))))
         (multiple-value-bind (sym ch) (scancode->keysym code *modifier-shift*)
           (when sym
             (let ((ev (make-input-event :type :key-down
                                         :key sym
                                         :char ch
                                         :modifiers (active-modifiers))))
               (enqueue-input-event ev))))))
     ;; Also trigger any legacy hooks
     (dolist (fn *key-hooks*)
       (ignore-errors (funcall fn code value))))))

(defun input-loop ()
  "Background epoll event loop reading evdev devices."
  (handler-case
      (let ((devices (find-input-event-devices)))
        (if (null devices)
            (format t "[INPUT] No evdev event devices found to monitor.~%")
            (format t "[INPUT] Monitoring devices: ~A~%" devices))
        (let ((epfd (%epoll-create1 0)))
          (when (< epfd 0)
            (format t "[INPUT] epoll_create1 failed: errno ~A~%" (sb-alien:get-errno))
            (return-from input-loop nil))
          (let ((open-fds '()))
            (unwind-protect
                 (progn
                   (cffi:with-foreign-pointer (ev 12)
                     (setf (cffi:mem-ref ev :uint32) +epollin+)
                     (dolist (dev devices)
                       (let ((fd (ignore-errors
                                  (sb-posix:open dev (logior sb-posix:o-rdonly sb-posix:o-nonblock)))))
                         (when (and fd (>= fd 0))
                           (push fd open-fds)
                           (setf (cffi:mem-ref (cffi:inc-pointer ev 4) :uint64) (logand #xFFFFFFFF fd))
                           (%epoll-ctl epfd +epoll-ctl-add+ fd ev)))))
                   (cffi:with-foreign-pointer (events-buf (* 12 16))
                     (cffi:with-foreign-pointer (event-frame 24)
                       (loop while *running-input-loop* do
                         (let ((n (%epoll-wait epfd events-buf 16 100)))
                          (when (> n 0)
                            (dotimes (i n)
                              (let* ((ev-ptr (cffi:inc-pointer events-buf (* i 12)))
                                     (fd (logand (cffi:mem-ref (cffi:inc-pointer ev-ptr 4) :uint64) #xFFFFFFFF)))
                                ;; Read all available 24-byte input_event packets from non-blocking fd
                                (loop
                                  (let ((bytes (%c-read fd event-frame 24)))
                                    (if (<= bytes 0)
                                        (return)
                                        (when (= bytes 24)
                                          (let ((type (cffi:mem-ref (cffi:inc-pointer event-frame 16) :uint16))
                                                (code (cffi:mem-ref (cffi:inc-pointer event-frame 18) :uint16))
                                                (val  (cffi:mem-ref (cffi:inc-pointer event-frame 20) :int32)))
                                            (handle-input-event type code val))))))))))))))
              (dolist (fd open-fds)
                (ignore-errors (sb-posix:close fd)))
              (ignore-errors (sb-posix:close epfd))))))
    (error (c)
      (format t "[INPUT] Warning: Input worker caught condition: ~A~%" c))))

(defun start-input-loop ()
  "Start the input worker thread."
  (unless *running-input-loop*
    (setf *running-input-loop* t)
    (setf *input-thread*
          (sb-thread:make-thread #'input-loop :name "LispOS-Input-Worker"))))

(defun stop-input-loop ()
  "Stop the input worker thread."
  (setf *running-input-loop* nil)
  (when *input-thread*
    (ignore-errors (sb-thread:terminate-thread *input-thread*))
    (setf *input-thread* nil)))
