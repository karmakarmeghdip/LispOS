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

(defgeneric buffer-mode-icon (buffer)
  (:documentation "Return character icon representing active buffer mode.")
  (:method ((buf buffer))
    (code-char #xF09A8)))

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
         (let ((all-leaves-fn (find-symbol "ALL-LEAF-WINDOWS" :lispos.window)))
           (when (and all-leaves-fn (fboundp all-leaves-fn))
             (dolist (win (funcall all-leaves-fn))
               (when (eq (lispos.window:window-buffer win) buf)
                 (setf (lispos.window:window-buffer win) replacement
                       (lispos.window:window-scroll-row win) 0
                       (lispos.window:window-scroll-col win) 0))))))
        t))))

;;; ---------------------------------------------------------------------------
;;; Factory Helper (Backward-Compatible)
;;; ---------------------------------------------------------------------------

(defun create-buffer (name &key (mode :shell) (engine nil) (file-path nil))
  "Create and register a buffer instance.
If a buffer with NAME already exists, returns it."
  (let ((existing (find-buffer name)))
    (if existing
        existing
        (let* ((shell-cls (find-class 'shell-buffer nil))
               (file-cls (find-class 'file-buffer nil))
               (editor-cls (find-class 'editor-buffer nil))
               (buf (case mode
                      (:shell
                       (if shell-cls
                           (make-instance shell-cls
                                          :name name
                                          :engine (or engine (lispos.shell:make-shell-engine)))
                           (make-instance 'buffer :name name)))
                      (otherwise
                       (cond
                         ((and file-cls file-path)
                          (make-instance file-cls :name name :file-path file-path))
                         (editor-cls
                          (make-instance editor-cls :name name))
                         (shell-cls
                          (make-instance shell-cls :name name))
                         (t
                          (make-instance 'buffer :name name)))))))
          (register-buffer buf)))))
