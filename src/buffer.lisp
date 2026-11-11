;;;; src/buffer.lisp - Universal Buffer Engine for LispOS

(in-package :lispos.buffer)

(defstruct buffer
  (id (gensym "BUF-") :type symbol)
  (name "*scratch*" :type string)
  (mode :scratch :type keyword)       ; :shell, :scratch, :file, :system
  (engine nil)                        ; backing engine (e.g. shell-engine or editor state)
  (file-path nil :type (or null string))
  (modified-p nil :type boolean)
  (read-only-p nil :type boolean)
  (lines (make-array 0 :adjustable t :fill-pointer t) :type vector)
  (point-row 0 :type fixnum)
  (point-col 0 :type fixnum))

(defvar *buffers* '()
  "Global list of all active buffers in most-recently-used order.")

(defvar *current-buffer* nil
  "The globally focused buffer.")

(defun list-buffers ()
  "Return list of all buffers."
  *buffers*)

(defun find-buffer (name)
  "Find a buffer by its name string."
  (find name *buffers* :key #'buffer-name :test #'string=))

(defun create-buffer (name &key (mode :scratch) (engine nil) (file-path nil))
  "Create and register a new buffer. If a buffer with NAME already exists, returns it."
  (let ((existing (find-buffer name)))
    (if existing
        existing
        (let ((buf (make-buffer :name name
                                :mode mode
                                :engine (or engine
                                            (case mode
                                              ((:shell :scratch)
                                               (lispos.shell:make-shell-engine))
                                              (t nil)))
                                :file-path file-path)))
          (setf *buffers* (nconc *buffers* (list buf)))
          (unless *current-buffer*
            (setf *current-buffer* buf))
          buf))))

(defun kill-buffer (buffer-or-name)
  "Remove buffer from active buffer list."
  (let ((buf (if (stringp buffer-or-name) (find-buffer buffer-or-name) buffer-or-name)))
    (when (and buf (> (length *buffers*) 1))
      (setf *buffers* (delete buf *buffers*))
      (when (eq *current-buffer* buf)
        (setf *current-buffer* (first *buffers*)))
      t)))

(defun buffer-content-lines (buf)
  "Return list or vector of text lines to display for BUF."
  (case (buffer-mode buf)
    ((:shell :scratch)
     (let ((eng (buffer-engine buf)))
       (if eng
           (let* ((history (lispos.shell:output-history-lines eng))
                  (prompt (lispos.shell:engine-prompt eng))
                  (buf-text (lispos.shell:buffer-text eng))
                  (current-line (concatenate 'string prompt buf-text)))
             (append history (list current-line)))
           '(""))))
    (t
     (coerce (buffer-lines buf) 'list))))

(defun buffer-cursor-position (buf)
  "Return (values row col) of the cursor within the buffer."
  (case (buffer-mode buf)
    ((:shell :scratch)
     (let ((eng (buffer-engine buf)))
       (if eng
           (let* ((history-count (length (lispos.shell:output-history-lines eng)))
                  (prompt-len (length (lispos.shell:engine-prompt eng)))
                  (point (lispos.shell:buffer-point eng)))
             (values history-count (+ prompt-len point)))
           (values 0 0))))
    (t
     (values (buffer-point-row buf) (buffer-point-col buf)))))

(defun buffer-handle-key (buf key &key (char nil) (modifiers nil))
  "Dispatch key event to buffer's backing engine."
  (case (buffer-mode buf)
    ((:shell :scratch)
     (let ((eng (buffer-engine buf)))
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
    (t
     ;; Basic editor buffer insertion
     (when (characterp (or char key))
       (format t "[BUF] Edit: ~A~%" (or char key))))))
