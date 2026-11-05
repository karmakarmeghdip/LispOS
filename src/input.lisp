;;;; src/input.lisp - evdev Input and epoll Event Loop for LispOS

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

(defvar *input-thread* nil)
(defvar *running-input-loop* nil)
(defvar *key-hooks* nil)
(defvar *pointer-hooks* nil)

(defvar *cursor-x* 100)
(defvar *cursor-y* 100)

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
     (dolist (fn *key-hooks*)
       (ignore-errors (funcall fn code value))))))

(defun input-loop ()
  "Background epoll event loop reading evdev devices."
  (let ((devices (find-input-event-devices)))
    (if (null devices)
        (format t "[INPUT] No evdev event devices found to monitor.~%")
        (format t "[INPUT] Monitoring devices: ~A~%" devices)))
  (let ((epfd (%epoll-create1 0)))
    (when (< epfd 0)
      (format t "[INPUT] epoll_create1 failed: errno ~A~%" (sb-alien:get-errno))
      (return-from input-loop nil))
    (let ((open-fds '()))
      (unwind-protect
           (progn
             (cffi:with-foreign-pointer (ev 12)
               (setf (cffi:mem-ref ev :uint32) +epollin+)
               (dolist (dev (find-input-event-devices))
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
                           ;; Read all available 24-byte input_event packets from fd
                           (loop
                             (let ((bytes (sb-posix:read fd event-frame 24)))
                               (if (or (null bytes) (/= bytes 24))
                                   (return)
                                   (let ((type (cffi:mem-ref (cffi:inc-pointer event-frame 16) :uint16))
                                         (code (cffi:mem-ref (cffi:inc-pointer event-frame 18) :uint16))
                                         (val  (cffi:mem-ref (cffi:inc-pointer event-frame 20) :int32)))
                                     (handle-input-event type code val)))))))))))))
        (dolist (fd open-fds)
          (ignore-errors (sb-posix:close fd)))
        (ignore-errors (sb-posix:close epfd))))))

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
