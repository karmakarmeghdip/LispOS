;;;; src/shell/line-editor.lisp - Structural Auto-Pairing & Line Editing Operations

(in-package :lispos.shell)

(defun buffer-text (engine)
  "Return current buffer text as a fresh string."
  (copy-seq (shell-engine-buffer engine)))

(defun buffer-point (engine)
  "Return current cursor point in buffer."
  (shell-engine-point engine))

(defun set-buffer-point (engine new-point)
  "Set cursor point safely clamped between 0 and buffer length."
  (let ((len (length (shell-engine-buffer engine))))
    (setf (shell-engine-point engine) (max 0 (min new-point len)))))

(defun %set-buffer-from-string (engine text)
  "Internal helper to replace buffer contents with TEXT."
  (let ((buf (shell-engine-buffer engine))
        (len (length text)))
    (adjust-array buf len :fill-pointer len)
    (loop for i from 0 below len do
      (setf (aref buf i) (char text i)))))

(defun set-buffer-text (engine text &optional new-point)
  "Set buffer text to TEXT and update point (defaults to end of TEXT)."
  (%set-buffer-from-string engine text)
  (set-buffer-point engine (or new-point (length text))))

(defun %buffer-insert-string (engine str)
  "Insert string STR at current point and advance point."
  (let* ((buf (shell-engine-buffer engine))
         (pt (shell-engine-point engine))
         (slen (length str))
         (old-len (length buf))
         (new-len (+ old-len slen)))
    (adjust-array buf new-len :fill-pointer new-len)
    ;; Shift characters right starting from the end
    (loop for i from (1- new-len) downto (+ pt slen) do
      (setf (aref buf i) (aref buf (- i slen))))
    ;; Insert new characters
    (loop for i from 0 below slen do
      (setf (aref buf (+ pt i)) (char str i)))
    (setf (shell-engine-point engine) (+ pt slen))))

(defun %buffer-insert-char (engine char)
  "Insert single character CHAR at current point."
  (%buffer-insert-string engine (string char)))

(defun %buffer-delete (engine start count)
  "Delete COUNT characters starting from index START."
  (let* ((buf (shell-engine-buffer engine))
         (len (length buf))
         (end (+ start count))
         (shift-count (- len end)))
    (when (and (>= start 0) (> count 0) (<= end len))
      (loop for i from 0 below shift-count do
        (setf (aref buf (+ start i)) (aref buf (+ end i))))
      (decf (fill-pointer buf) count))))

(defun handle-char (engine char)
  "Insert character into buffer with structural auto-pairing logic:
- Typing `(` inserts `()` and places the cursor between them.
- Typing `)` over an existing `)` steps over it."
  (let ((buf (shell-engine-buffer engine))
        (pt (shell-engine-point engine)))
    (cond
      ;; Structural auto-pairing: typing `(` inserts `()` and places cursor between
      ((char= char #\()
       (%buffer-insert-string engine "()")
       (setf (shell-engine-point engine) (1+ pt))
       :insert-pair)

      ;; Structural auto-pairing: typing `)` over existing `)` steps over it
      ((char= char #\))
       (if (and (< pt (length buf))
                (char= (aref buf pt) #\)))
           (progn
             (incf (shell-engine-point engine))
             :step-over)
           (progn
             (%buffer-insert-char engine #\))
             :insert)))

      ;; Normal character insertion
      (t
       (%buffer-insert-char engine char)
       :insert))))

(defun handle-backspace (engine)
  "Delete character before point. If between `()`, delete both."
  (let ((buf (shell-engine-buffer engine))
        (pt (shell-engine-point engine)))
    (when (> pt 0)
      (if (and (< pt (length buf))
               (char= (aref buf (1- pt)) #\()
               (char= (aref buf pt) #\)))
          (progn
            ;; Between `()`, delete both
            (%buffer-delete engine (1- pt) 2)
            (setf (shell-engine-point engine) (1- pt))
            :delete-pair)
          (progn
            ;; Delete single character before point
            (%buffer-delete engine (1- pt) 1)
            (setf (shell-engine-point engine) (1- pt))
            :delete)))))

(defun handle-delete (engine)
  "Delete character under point (Delete / C-d)."
  (let ((buf (shell-engine-buffer engine))
        (pt (shell-engine-point engine)))
    (when (< pt (length buf))
      (%buffer-delete engine pt 1)
      :delete)))

(defun handle-kill-to-end (engine)
  "Delete text from point to end of buffer (Emacs C-k)."
  (let* ((buf (shell-engine-buffer engine))
         (pt (shell-engine-point engine))
         (len (length buf)))
    (when (< pt len)
      (%buffer-delete engine pt (- len pt))
      :killed)))

(defun handle-kill-line (engine)
  "Kill from start of line to point, or clear entire line if at start (Emacs C-u)."
  (let ((pt (shell-engine-point engine)))
    (if (> pt 0)
        (progn
          (%buffer-delete engine 0 pt)
          (setf (shell-engine-point engine) 0)
          :killed)
        (progn
          (setf (fill-pointer (shell-engine-buffer engine)) 0)
          (setf (shell-engine-point engine) 0)
          :killed))))

(defun handle-clear-screen (engine)
  "Clear output history (Emacs C-l)."
  (setf (shell-engine-output-history engine) '())
  :cleared)

(defun handle-left (engine)
  "Move point left by 1 (Left arrow / C-b)."
  (let ((pt (shell-engine-point engine)))
    (if (> pt 0)
        (progn
          (setf (shell-engine-point engine) (1- pt))
          :moved)
        :boundary)))

(defun handle-right (engine)
  "Move point right by 1 (Right arrow / C-f)."
  (let ((pt (shell-engine-point engine))
        (len (length (shell-engine-buffer engine))))
    (if (< pt len)
        (progn
          (setf (shell-engine-point engine) (1+ pt))
          :moved)
        :boundary)))

(defun handle-home (engine)
  "Move point to start of line (Home / C-a)."
  (setf (shell-engine-point engine) 0)
  :moved)

(defun handle-end (engine)
  "Move point to end of line (End / C-e)."
  (setf (shell-engine-point engine) (length (shell-engine-buffer engine)))
  :moved)
