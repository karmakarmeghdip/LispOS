;;;; src/ui/buffer.lisp - Polymorphic CLOS Buffer Foundation for LispOS

(in-package :lispos.buffer)

;;; ---------------------------------------------------------------------------
;;; Base Buffer Class
;;; ---------------------------------------------------------------------------

(defclass buffer ()
  ((id
    :initarg :id
    :initform (gensym "BUF-")
    :reader buffer-id
    :type symbol
    :documentation "Unique identifier symbol for this buffer instance.")
   (name
    :initarg :name
    :initform "*unnamed*"
    :accessor buffer-name
    :type string
    :documentation "Human-readable display name of the buffer.")
   (modified-p
    :initarg :modified-p
    :initform nil
    :accessor buffer-modified-p
    :type boolean
    :documentation "True if in-memory content has diverged from saved state.")
   (read-only-p
    :initarg :read-only-p
    :initform nil
    :accessor buffer-read-only-p
    :type boolean
    :documentation "True if user modifications are blocked."))
  (:documentation "Abstract base class for all in-memory display buffers."))

(defun buffer-p (obj)
  "Type predicate for buffer instances."
  (typep obj 'buffer))

;; Default implementations / fallbacks for optional slots
(defgeneric buffer-file-path (buffer)
  (:documentation "Return file pathname string if buffer is backed by disk, else NIL.")
  (:method ((buf buffer)) nil))

(defgeneric buffer-engine (buffer)
  (:documentation "Return underlying shell-engine if shell buffer, else NIL.")
  (:method ((buf buffer)) nil))

(defgeneric buffer-mode (buffer)
  (:documentation "Return mode keyword for legacy callers (:shell, :editor, :file).")
  (:method ((buf buffer)) :custom))

;;; ---------------------------------------------------------------------------
;;; Minimal Polymorphic Buffer Protocol
;;; ---------------------------------------------------------------------------

(defgeneric buffer-content-lines (buffer)
  (:documentation "Return a list or vector of string lines to render in viewport."))

(defgeneric buffer-cursor-position (buffer)
  (:documentation "Return (values row col) where row is 0-indexed line in content-lines,
and col is 0-indexed character offset within that line."))

(defgeneric buffer-mode-name (buffer)
  (:documentation "Return short status string representing active mode (e.g. 'NOR', 'INS', 'SHELL')."))

(defgeneric buffer-handle-key (buffer key &key char modifiers)
  (:documentation "Handle raw keystroke dispatched from window/event loop."))

(defgeneric buffer-modeline-details (buffer)
  (:documentation "Return extra contextual string for modeline display (path, status, etc.).")
  (:method ((buf buffer)) ""))

(defgeneric save-buffer (buffer &optional path)
  (:documentation "Persist buffer content to disk. Default method is a safe no-op.")
  (:method ((buf buffer) &optional path)
    (declare (ignore path))
    nil))

;;; ---------------------------------------------------------------------------
;;; Global Buffer Registry & Lifecycle
;;; ---------------------------------------------------------------------------

(defvar *buffers* '()
  "Global list of all active buffers in most-recently-used order.")

(defvar *current-buffer* nil
  "The globally focused buffer.")

(defun list-buffers ()
  "Return list of all active buffers."
  *buffers*)

(defun find-buffer (name)
  "Find an active buffer by name string."
  (find name *buffers* :key #'buffer-name :test #'string=))

(defun register-buffer (buf)
  "Register BUF in active buffer list if not already present, making it current if none."
  (unless (member buf *buffers*)
    (setf *buffers* (nconc *buffers* (list buf))))
  (unless *current-buffer*
    (setf *current-buffer* buf))
  buf)

(defun kill-buffer (buffer-or-name)
  "Remove buffer from active buffer list and focus an adjacent buffer if needed."
  (let ((buf (if (stringp buffer-or-name) (find-buffer buffer-or-name) buffer-or-name)))
    (when (and buf (> (length *buffers*) 1))
      (setf *buffers* (delete buf *buffers*))
      (let ((replacement (or (find-if (lambda (b) (not (eq b buf))) *buffers*)
                             *current-buffer*)))
        (when (eq *current-buffer* buf)
          (setf *current-buffer* replacement))
        ;; Safely update any window displaying the killed buffer
        (ignore-errors
         (let ((all-leaves-fn (find-symbol "ALL-LEAF-WINDOWS" :lispos.window))
               (win-buf-fn (find-symbol "WINDOW-BUFFER" :lispos.window)))
           (when (and all-leaves-fn (fboundp all-leaves-fn) win-buf-fn)
             (dolist (win (funcall all-leaves-fn))
               (when (eq (funcall win-buf-fn win) buf)
                 (setf (slot-value win (find-symbol "BUFFER" :lispos.window)) replacement
                       (slot-value win (find-symbol "SCROLL-ROW" :lispos.window)) 0
                       (slot-value win (find-symbol "SCROLL-COL" :lispos.window)) 0))))))
        t))))

;;; ---------------------------------------------------------------------------
;;; Concrete Subclass: Shell Buffer
;;; ---------------------------------------------------------------------------

(defclass shell-buffer (buffer)
  ((engine
    :initarg :engine
    :initform nil
    :accessor shell-buffer-engine
    :documentation "Underlying shell-engine instance."))
  (:default-initargs :name "*shell*")
  (:documentation "Buffer hosting an interactive LispOS shell REPL session."))

(defmethod initialize-instance :after ((buf shell-buffer) &key &allow-other-keys)
  (unless (shell-buffer-engine buf)
    (setf (shell-buffer-engine buf) (lispos.shell:make-shell-engine))))

(defmethod buffer-engine ((buf shell-buffer))
  (shell-buffer-engine buf))

(defmethod buffer-mode ((buf shell-buffer))
  :shell)

(defmethod buffer-mode-name ((buf shell-buffer))
  "SHELL")

(defmethod buffer-content-lines ((buf shell-buffer))
  (let ((eng (shell-buffer-engine buf)))
    (if eng
        (let* ((history (lispos.shell:output-history-lines eng))
               (prompt (lispos.shell:engine-prompt eng))
               (buf-text (lispos.shell:buffer-text eng))
               (current-line (concatenate 'string prompt buf-text)))
          (append history (list current-line)))
        '(""))))

(defmethod buffer-cursor-position ((buf shell-buffer))
  (let ((eng (shell-buffer-engine buf)))
    (if eng
        (let* ((history-count (length (lispos.shell:output-history-lines eng)))
               (prompt-len (length (lispos.shell:engine-prompt eng)))
               (point (lispos.shell:buffer-point eng)))
          (values history-count (+ prompt-len point)))
        (values 0 0))))

(defmethod buffer-handle-key ((buf shell-buffer) key &key (char nil) (modifiers nil))
  (let ((eng (shell-buffer-engine buf)))
    (when eng
      (let ((effective-key
              (cond
                ((and (member :ctrl modifiers) (characterp (or char key)))
                 (let ((c (char-upcase (or char key))))
                   (case c
                     (#\A :ctrl-a)
                     (#\B :ctrl-b)
                     (#\C :ctrl-c)
                     (#\D :ctrl-d)
                     (#\E :ctrl-e)
                     (#\F :ctrl-f)
                     (#\K :ctrl-k)
                     (#\L :ctrl-l)
                     (#\N :ctrl-n)
                     (#\P :ctrl-p)
                     (#\U :ctrl-u)
                     (t key))))
                (t (or char key)))))
        (lispos.shell:handle-key eng effective-key)))))

(defmethod buffer-modeline-details ((buf shell-buffer))
  (let* ((cwd (or (ignore-errors (lispos.shell:get-cwd-namestring)) "~"))
         (admin-home (ignore-errors (namestring (pathname lispos.paths:+admin-home+))))
         (clean-home (if (and admin-home (> (length admin-home) 1)
                              (char= (char admin-home (1- (length admin-home))) #\/))
                         (subseq admin-home 0 (1- (length admin-home)))
                         admin-home)))
    (cond
      ((or (string= cwd clean-home) (string= cwd "/Users/Admin") (string= cwd "/Users/Admin/")) "~")
      ((and clean-home (search clean-home cwd) (= (search clean-home cwd) 0))
       (concatenate 'string "~" (subseq cwd (length clean-home))))
      (t cwd))))

;;; ---------------------------------------------------------------------------
;;; Factory Helper (Backward-Compatible)
;;; ---------------------------------------------------------------------------

(defun create-buffer (name &key (mode :shell) (engine nil) (file-path nil))
  "Create and register a buffer instance.
If a buffer with NAME already exists, returns it."
  (let ((existing (find-buffer name)))
    (if existing
        existing
        (let ((buf (case mode
                     (:shell
                      (make-instance 'shell-buffer
                                     :name name
                                     :engine (or engine (lispos.shell:make-shell-engine))))
                     (otherwise
                      ;; If editor classes are loaded, instantiated via editor-buffer / file-buffer
                      (if (find-class 'file-buffer nil)
                          (if file-path
                              (make-instance 'file-buffer :name name :file-path file-path)
                              (make-instance 'editor-buffer :name name))
                          (make-instance 'shell-buffer :name name))))))
          (register-buffer buf)))))
