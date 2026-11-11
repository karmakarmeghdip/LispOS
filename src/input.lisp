;;;; src/input.lisp - evdev Input, Keysym Translation, and Event Queue for LispOS

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

;; Standard Linux evdev key scancodes
(defconstant +key-esc+ 1)
(defconstant +key-1+ 2)
(defconstant +key-2+ 3)
(defconstant +key-3+ 4)
(defconstant +key-4+ 5)
(defconstant +key-5+ 6)
(defconstant +key-6+ 7)
(defconstant +key-7+ 8)
(defconstant +key-8+ 9)
(defconstant +key-9+ 10)
(defconstant +key-0+ 11)
(defconstant +key-minus+ 12)
(defconstant +key-equal+ 13)
(defconstant +key-backspace+ 14)
(defconstant +key-tab+ 15)
(defconstant +key-q+ 16)
(defconstant +key-w+ 17)
(defconstant +key-e+ 18)
(defconstant +key-r+ 19)
(defconstant +key-t+ 20)
(defconstant +key-y+ 21)
(defconstant +key-u+ 22)
(defconstant +key-i+ 23)
(defconstant +key-o+ 24)
(defconstant +key-p+ 25)
(defconstant +key-leftbrace+ 26)
(defconstant +key-rightbrace+ 27)
(defconstant +key-enter+ 28)
(defconstant +key-leftctrl+ 29)
(defconstant +key-a+ 30)
(defconstant +key-s+ 31)
(defconstant +key-d+ 32)
(defconstant +key-f+ 33)
(defconstant +key-g+ 34)
(defconstant +key-h+ 35)
(defconstant +key-j+ 36)
(defconstant +key-k+ 37)
(defconstant +key-l+ 38)
(defconstant +key-semicolon+ 39)
(defconstant +key-apostrophe+ 40)
(defconstant +key-grave+ 41)
(defconstant +key-leftshift+ 42)
(defconstant +key-backslash+ 43)
(defconstant +key-z+ 44)
(defconstant +key-x+ 45)
(defconstant +key-c+ 46)
(defconstant +key-v+ 47)
(defconstant +key-b+ 48)
(defconstant +key-n+ 49)
(defconstant +key-m+ 50)
(defconstant +key-comma+ 51)
(defconstant +key-dot+ 52)
(defconstant +key-slash+ 53)
(defconstant +key-rightshift+ 54)
(defconstant +key-kpasterisk+ 55)
(defconstant +key-leftalt+ 56)
(defconstant +key-space+ 57)
(defconstant +key-capslock+ 58)
(defconstant +key-kpenter+ 96)
(defconstant +key-rightctrl+ 97)
(defconstant +key-rightalt+ 100)
(defconstant +key-home+ 102)
(defconstant +key-up+ 103)
(defconstant +key-pageup+ 104)
(defconstant +key-left+ 105)
(defconstant +key-right+ 106)
(defconstant +key-end+ 107)
(defconstant +key-down+ 108)
(defconstant +key-pagedown+ 109)
(defconstant +key-insert+ 110)
(defconstant +key-delete+ 111)
(defconstant +key-leftmeta+ 125)
(defconstant +key-rightmeta+ 126)

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

(defstruct input-event
  (type :key-down)  ; :key-down, :key-up, :pointer
  (key nil)         ; :left, :right, :up, :down, :enter, :backspace, or char
  (char nil)        ; Character if printable, or nil
  (modifiers nil))  ; List of active modifiers, e.g. (:ctrl :shift :super)

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

(defun enqueue-input-event (event)
  "Push event to the synchronized event queue."
  (sb-thread:with-mutex (*event-lock*)
    (setf *event-queue* (nconc *event-queue* (list event)))))

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

(defun scancode->keysym (code shift-p)
  "Convert Linux evdev scancode to (values key-symbol char-or-nil)."
  (case code
    ;; Navigation / Control Keys
    (103 (values :up nil))
    (108 (values :down nil))
    (105 (values :left nil))
    (106 (values :right nil))
    ((28 96) (values :enter #\Newline))
    (14  (values :backspace nil))
    (111 (values :delete nil))
    (102 (values :home nil))
    (107 (values :end nil))
    (15  (values :tab #\Tab))
    (1   (values :escape nil))
    (57  (values #\Space #\Space))
    ;; Numbers & Symbols row
    (2   (if shift-p (values #\! #\!) (values #\1 #\1)))
    (3   (if shift-p (values #\@ #\@) (values #\2 #\2)))
    (4   (if shift-p (values #\# #\#) (values #\3 #\3)))
    (5   (if shift-p (values #\$ #\$) (values #\4 #\4)))
    (6   (if shift-p (values #\% #\%) (values #\5 #\5)))
    (7   (if shift-p (values #\^ #\^) (values #\6 #\6)))
    (8   (if shift-p (values #\& #\&) (values #\7 #\7)))
    (9   (if shift-p (values #\* #\*) (values #\8 #\8)))
    (10  (if shift-p (values #\( #\() (values #\9 #\9)))
    (11  (if shift-p (values #\) #\)) (values #\0 #\0)))
    (12  (if shift-p (values #\_ #\_) (values #\- #\-)))
    (13  (if shift-p (values #\+ #\+) (values #\= #\=)))
    ;; Letters
    (30  (if shift-p (values #\A #\A) (values #\a #\a)))
    (48  (if shift-p (values #\B #\B) (values #\b #\b)))
    (46  (if shift-p (values #\C #\C) (values #\c #\c)))
    (32  (if shift-p (values #\D #\D) (values #\d #\d)))
    (18  (if shift-p (values #\E #\E) (values #\e #\e)))
    (33  (if shift-p (values #\F #\F) (values #\f #\f)))
    (34  (if shift-p (values #\G #\G) (values #\g #\g)))
    (35  (if shift-p (values #\H #\H) (values #\h #\h)))
    (23  (if shift-p (values #\I #\I) (values #\i #\i)))
    (36  (if shift-p (values #\J #\J) (values #\j #\j)))
    (37  (if shift-p (values #\K #\K) (values #\k #\k)))
    (38  (if shift-p (values #\L #\L) (values #\l #\l)))
    (50  (if shift-p (values #\M #\M) (values #\m #\m)))
    (49  (if shift-p (values #\N #\N) (values #\n #\n)))
    (24  (if shift-p (values #\O #\O) (values #\o #\o)))
    (25  (if shift-p (values #\P #\P) (values #\p #\p)))
    (16  (if shift-p (values #\Q #\Q) (values #\q #\q)))
    (19  (if shift-p (values #\R #\R) (values #\r #\r)))
    (31  (if shift-p (values #\S #\S) (values #\s #\s)))
    (20  (if shift-p (values #\T #\T) (values #\t #\t)))
    (22  (if shift-p (values #\U #\U) (values #\u #\u)))
    (47  (if shift-p (values #\V #\V) (values #\v #\v)))
    (17  (if shift-p (values #\W #\W) (values #\w #\w)))
    (45  (if shift-p (values #\X #\X) (values #\x #\x)))
    (21  (if shift-p (values #\Y #\Y) (values #\y #\y)))
    (44  (if shift-p (values #\Z #\Z) (values #\z #\z)))
    ;; Punctuation
    (26  (if shift-p (values #\{ #\{) (values #\[ #\[)))
    (27  (if shift-p (values #\} #\}) (values #\] #\])))
    (39  (if shift-p (values #\: #\:) (values #\; #\;)))
    (40  (if shift-p (values #\" #\") (values #\' #\')))
    (41  (if shift-p (values #\~ #\~) (values #\` #\`)))
    (43  (if shift-p (values #\| #\|) (values #\\ #\\)))
    (51  (if shift-p (values #\< #\<) (values #\, #\,)))
    (52  (if shift-p (values #\> #\>) (values #\. #\.)))
    (53  (if shift-p (values #\? #\?) (values #\/ #\/)))
    (t (values nil nil))))

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
