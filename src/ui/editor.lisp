;;;; src/ui/editor.lisp - Helix Modal Editor Buffers (Normal/Insert/Command/Select) for LispOS

(in-package :lispos.buffer)

;;; ---------------------------------------------------------------------------
;;; Editor Buffer Base Class
;;; ---------------------------------------------------------------------------

(defclass editor-buffer (buffer)
  ((lines
    :initarg :lines
    :initform (make-array 1 :adjustable t :fill-pointer 1 :initial-contents (list ""))
    :accessor editor-buffer-lines
    :type vector
    :documentation "Adjustable vector of string lines representing buffer text.")
   (cursor-row
    :initarg :cursor-row
    :initform 0
    :accessor editor-buffer-cursor-row
    :type fixnum
    :documentation "0-indexed cursor row in lines.")
   (cursor-col
    :initarg :cursor-col
    :initform 0
    :accessor editor-buffer-cursor-col
    :type fixnum
    :documentation "0-indexed cursor column in current line.")
   (helix-mode
    :initarg :helix-mode
    :initform :normal
    :accessor editor-buffer-helix-mode
    :type (member :normal :insert :command :select)
    :documentation "Active Helix editing mode (:normal, :insert, :command, :select).")
   (selection-anchor-row
    :initform nil
    :accessor editor-buffer-selection-anchor-row
    :type (or null fixnum)
    :documentation "Row where visual selection began, or NIL if inactive.")
   (selection-anchor-col
    :initform nil
    :accessor editor-buffer-selection-anchor-col
    :type (or null fixnum)
    :documentation "Col where visual selection began, or NIL if inactive.")
   (command-input
    :initform ""
    :accessor editor-buffer-command-input
    :type string
    :documentation "Buffer for user typing during Helix command mode (:w, :q, etc.).")
   (status-message
    :initform nil
    :accessor editor-buffer-status-message
    :type (or null string)
    :documentation "Transient status message displayed in modeline.")
   (pending-command
    :initform nil
    :accessor editor-buffer-pending-command
    :documentation "Pending multi-key command (:go, :find-char-fwd, :replace-char, etc.).")
   (leader-active-p
    :initform nil
    :accessor editor-buffer-leader-active-p
    :type boolean
    :documentation "True when Space leader key is active awaiting next command.")
   (clipboard
    :initform ""
    :accessor editor-buffer-clipboard
    :type string
    :documentation "Internal clipboard register for yank and paste.")
   (undo-stack
    :initform '()
    :accessor editor-buffer-undo-stack
    :type list
    :documentation "History stack of prior editor states.")
   (redo-stack
    :initform '()
    :accessor editor-buffer-redo-stack
    :type list
    :documentation "History stack of undone states for redo.")
   (last-key
    :initform nil
    :accessor editor-buffer-last-key
    :documentation "Last key recorded for multi-key sequences."))
  (:default-initargs :name "*editor*")
  (:documentation "Buffer featuring Helix modal editing (Normal, Insert, Command, Select)."))

(defmethod buffer-mode ((buf editor-buffer))
  :editor)

(defmethod buffer-mode-name ((buf editor-buffer))
  (case (editor-buffer-helix-mode buf)
    (:normal  "NOR")
    (:insert  "INS")
    (:command "CMD")
    (:select  "SEL")
    (t        "EDT")))

(defmethod buffer-content-lines ((buf editor-buffer))
  (let ((base-lines (coerce (editor-buffer-lines buf) 'list)))
    (if (eq (editor-buffer-helix-mode buf) :command)
        (append base-lines (list (format nil ":~A" (editor-buffer-command-input buf))))
        base-lines)))

(defmethod buffer-cursor-position ((buf editor-buffer))
  (if (eq (editor-buffer-helix-mode buf) :command)
      (values (length (editor-buffer-lines buf))
              (1+ (length (editor-buffer-command-input buf))))
      (values (editor-buffer-cursor-row buf)
              (editor-buffer-cursor-col buf))))

(defmethod buffer-modeline-details ((buf editor-buffer))
  (cond
    ((editor-buffer-leader-active-p buf)
     "[SPC] f: file  b: buffer  w: save  q: quit  s: scratch")
    ((editor-buffer-status-message buf)
     (editor-buffer-status-message buf))
    (t
     (format nil "L~D:C~D"
             (1+ (editor-buffer-cursor-row buf))
             (1+ (editor-buffer-cursor-col buf))))))

;;; ---------------------------------------------------------------------------
;;; Undo / Redo Subsystem
;;; ---------------------------------------------------------------------------

(defun %clone-lines (lines)
  "Create an adjustable vector with copies of all strings in LINES."
  (let* ((len (length lines))
         (new-arr (make-array (max 1 len) :adjustable t :fill-pointer len)))
    (loop for i from 0 below len do
      (setf (aref new-arr i) (copy-seq (aref lines i))))
    new-arr))

(defun %push-undo-snapshot (buf)
  "Record snapshot of current editor text and cursor for undo."
  (let ((entry (list (%clone-lines (editor-buffer-lines buf))
                     (editor-buffer-cursor-row buf)
                     (editor-buffer-cursor-col buf))))
    (push entry (editor-buffer-undo-stack buf))
    (setf (editor-buffer-redo-stack buf) nil)
    ;; Limit history depth to 64 states
    (when (> (length (editor-buffer-undo-stack buf)) 64)
      (setf (cdr (nthcdr 63 (editor-buffer-undo-stack buf))) nil))))

(defun %undo (buf)
  "Restore previous editor state from undo stack."
  (if (editor-buffer-undo-stack buf)
      (let ((current (list (%clone-lines (editor-buffer-lines buf))
                           (editor-buffer-cursor-row buf)
                           (editor-buffer-cursor-col buf)))
            (prev (pop (editor-buffer-undo-stack buf))))
        (push current (editor-buffer-redo-stack buf))
        (setf (editor-buffer-lines buf) (first prev)
              (editor-buffer-cursor-row buf) (second prev)
              (editor-buffer-cursor-col buf) (third prev)
              (buffer-modified-p buf) t
              (editor-buffer-status-message buf) "[Undo]")
        (%clamp-cursor buf)
        t)
      (progn
        (setf (editor-buffer-status-message buf) "[Already at oldest change]")
        nil)))

(defun %redo (buf)
  "Restore undone editor state from redo stack."
  (if (editor-buffer-redo-stack buf)
      (let ((current (list (%clone-lines (editor-buffer-lines buf))
                           (editor-buffer-cursor-row buf)
                           (editor-buffer-cursor-col buf)))
            (next (pop (editor-buffer-redo-stack buf))))
        (push current (editor-buffer-undo-stack buf))
        (setf (editor-buffer-lines buf) (first next)
              (editor-buffer-cursor-row buf) (second next)
              (editor-buffer-cursor-col buf) (third next)
              (buffer-modified-p buf) t
              (editor-buffer-status-message buf) "[Redo]")
        (%clamp-cursor buf)
        t)
      (progn
        (setf (editor-buffer-status-message buf) "[Already at newest change]")
        nil)))

;;; ---------------------------------------------------------------------------
;;; Selection Subsystem (Helix Visual Selection)
;;; ---------------------------------------------------------------------------

(defun %get-selection-bounds (buf)
  "Return (values start-row start-col end-row end-col) ordered."
  (let ((ar (or (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)))
        (ac (or (editor-buffer-selection-anchor-col buf) (editor-buffer-cursor-col buf)))
        (cr (editor-buffer-cursor-row buf))
        (cc (editor-buffer-cursor-col buf)))
    (if (or (< ar cr) (and (= ar cr) (<= ac cc)))
        (values ar ac cr cc)
        (values cr cc ar ac))))

(defun %extract-selection-text (buf)
  "Extract text in selection bounds as a string."
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (let ((lines (editor-buffer-lines buf)))
      (cond
        ((= s-row e-row)
         (let* ((line (aref lines s-row))
                (c1 (min s-col (length line)))
                (c2 (min e-col (length line))))
           (if (< c1 c2) (subseq line c1 c2) "")))
        (t
         (with-output-to-string (s)
           (let* ((first-line (aref lines s-row))
                  (c1 (min s-col (length first-line))))
             (write-line (subseq first-line c1) s))
           (loop for r from (1+ s-row) below e-row do
             (write-line (aref lines r) s))
           (let* ((last-line (aref lines e-row))
                  (c2 (min e-col (length last-line))))
             (write-string (subseq last-line 0 c2) s))))))))

(defun %delete-selection (buf)
  "Delete text in selection bounds and return to normal mode."
  (%push-undo-snapshot buf)
  (let ((clip (%extract-selection-text buf)))
    (setf (editor-buffer-clipboard buf) clip))
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (let ((lines (editor-buffer-lines buf)))
      (cond
        ((= s-row e-row)
         (let ((line (aref lines s-row)))
           (if (and (= s-col 0) (>= e-col (1- (length line))))
               ;; Entire line selected
               (if (> (length lines) 1)
                   (progn
                     (loop for i from s-row below (1- (length lines)) do
                       (setf (aref lines i) (aref lines (1+ i))))
                     (decf (fill-pointer lines))
                     (setf (editor-buffer-cursor-row buf) (max 0 (min s-row (1- (length lines))))
                           (editor-buffer-cursor-col buf) 0))
                   (progn
                     (setf (aref lines 0) ""
                           (editor-buffer-cursor-col buf) 0)))
               ;; Partial line deletion
               (let* ((c1 (min s-col (length line)))
                      (c2 (min e-col (length line)))
                      (new-line (concatenate 'string (subseq line 0 c1) (subseq line c2))))
                 (setf (aref lines s-row) new-line
                       (editor-buffer-cursor-row buf) s-row
                       (editor-buffer-cursor-col buf) c1)))))
        (t
         (let* ((first-line (aref lines s-row))
                (last-line (aref lines e-row))
                (c1 (min s-col (length first-line)))
                (c2 (min e-col (length last-line)))
                (merged-line (concatenate 'string (subseq first-line 0 c1) (subseq last-line c2)))
                (deleted-count (- e-row s-row))
                (len (length lines)))
           (setf (aref lines s-row) merged-line)
           (loop for i from (1+ s-row) below (- len deleted-count) do
             (setf (aref lines i) (aref lines (+ i deleted-count))))
           (decf (fill-pointer lines) deleted-count)
           (setf (editor-buffer-cursor-row buf) s-row
                 (editor-buffer-cursor-col buf) c1))))))
  (setf (editor-buffer-helix-mode buf) :normal
        (editor-buffer-selection-anchor-row buf) nil
        (editor-buffer-selection-anchor-col buf) nil
        (buffer-modified-p buf) t)
  (%clamp-cursor buf))

;;; ---------------------------------------------------------------------------
;;; Line Editing Primitives
;;; ---------------------------------------------------------------------------

(defun %current-line (buf)
  (let ((lines (editor-buffer-lines buf))
        (r (editor-buffer-cursor-row buf)))
    (if (and (>= r 0) (< r (length lines)))
        (aref lines r)
        "")))

(defun %set-current-line (buf str)
  (let ((lines (editor-buffer-lines buf))
        (r (editor-buffer-cursor-row buf)))
    (when (and (>= r 0) (< r (length lines)))
      (setf (aref lines r) str))))

(defun %clamp-cursor (buf)
  "Keep cursor row and column safely within bounds."
  (let* ((lines (editor-buffer-lines buf))
         (max-r (max 0 (1- (length lines)))))
    (setf (editor-buffer-cursor-row buf) (max 0 (min (editor-buffer-cursor-row buf) max-r)))
    (let* ((cur (%current-line buf))
           (max-c (case (editor-buffer-helix-mode buf)
                    (:insert (length cur))
                    (:select (max 0 (length cur)))
                    (t (max 0 (1- (length cur)))))))
      (setf (editor-buffer-cursor-col buf) (max 0 (min (editor-buffer-cursor-col buf) max-c))))))

(defun %insert-char-at-point (buf char)
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf))
         (new-str (concatenate 'string
                               (subseq cur 0 (min col (length cur)))
                               (string char)
                               (subseq cur (min col (length cur))))))
    (%set-current-line buf new-str)
    (incf (editor-buffer-cursor-col buf))
    (setf (buffer-modified-p buf) t)))

(defun %split-line-at-point (buf)
  "Split line at cursor point on Enter."
  (%push-undo-snapshot buf)
  (let* ((lines (editor-buffer-lines buf))
         (row (editor-buffer-cursor-row buf))
         (col (editor-buffer-cursor-col buf))
         (cur (%current-line buf))
         (left (subseq cur 0 (min col (length cur))))
         (right (subseq cur (min col (length cur))))
         (new-len (1+ (length lines))))
    (%set-current-line buf left)
    (vector-push-extend "" lines)
    (loop for i from (1- new-len) downto (+ row 2) do
      (setf (aref lines i) (aref lines (1- i))))
    (setf (aref lines (1+ row)) right)
    (incf (editor-buffer-cursor-row buf))
    (setf (editor-buffer-cursor-col buf) 0)
    (setf (buffer-modified-p buf) t)))

(defun %delete-char-before-point (buf)
  "Handle Backspace in insert mode."
  (let ((col (editor-buffer-cursor-col buf))
        (row (editor-buffer-cursor-row buf))
        (lines (editor-buffer-lines buf)))
    (cond
      ((> col 0)
       (let* ((cur (%current-line buf))
              (new-str (concatenate 'string
                                    (subseq cur 0 (1- col))
                                    (subseq cur col))))
         (%set-current-line buf new-str)
         (decf (editor-buffer-cursor-col buf))
         (setf (buffer-modified-p buf) t)))
      ((and (= col 0) (> row 0))
       (%push-undo-snapshot buf)
       (let* ((prev (aref lines (1- row)))
              (cur (%current-line buf))
              (new-prev (concatenate 'string prev cur))
              (prev-len (length prev))
              (len (length lines)))
         (setf (aref lines (1- row)) new-prev)
         (loop for i from row below (1- len) do
           (setf (aref lines i) (aref lines (1+ i))))
         (decf (fill-pointer lines))
         (decf (editor-buffer-cursor-row buf))
         (setf (editor-buffer-cursor-col buf) prev-len)
         (setf (buffer-modified-p buf) t))))))

(defun %delete-char-under-point (buf)
  "Delete character under cursor point."
  (%push-undo-snapshot buf)
  (let* ((col (editor-buffer-cursor-col buf))
         (row (editor-buffer-cursor-row buf))
         (lines (editor-buffer-lines buf))
         (cur (%current-line buf)))
    (cond
      ((< col (length cur))
       (let ((new-str (concatenate 'string
                                   (subseq cur 0 col)
                                   (subseq cur (1+ col)))))
         (%set-current-line buf new-str)
         (%clamp-cursor buf)
         (setf (buffer-modified-p buf) t)))
      ((and (= col (length cur)) (< (1+ row) (length lines)))
       (let* ((next (aref lines (1+ row)))
              (new-cur (concatenate 'string cur next))
              (len (length lines)))
         (%set-current-line buf new-cur)
         (loop for i from (1+ row) below (1- len) do
           (setf (aref lines i) (aref lines (1+ i))))
         (decf (fill-pointer lines))
         (setf (buffer-modified-p buf) t))))))

(defun %delete-current-line (buf)
  "Delete current line in normal mode."
  (%push-undo-snapshot buf)
  (let ((lines (editor-buffer-lines buf))
        (row (editor-buffer-cursor-row buf)))
    (if (> (length lines) 1)
        (let ((len (length lines)))
          (loop for i from row below (1- len) do
            (setf (aref lines i) (aref lines (1+ i))))
          (decf (fill-pointer lines))
          (%clamp-cursor buf)
          (setf (buffer-modified-p buf) t))
        (progn
          (setf (aref lines 0) "")
          (setf (editor-buffer-cursor-col buf) 0)
          (setf (buffer-modified-p buf) t)))))

(defun %paste (buf &key (after t))
  "Paste contents of clipboard into buffer."
  (let ((clip (editor-buffer-clipboard buf)))
    (when (> (length clip) 0)
      (%push-undo-snapshot buf)
      (let* ((lines (editor-buffer-lines buf))
             (row (editor-buffer-cursor-row buf))
             (clip-lines (with-input-from-string (s clip)
                           (loop for line = (read-line s nil nil)
                                 while line collect line)))
             (num-new (max 1 (length clip-lines)))
             (insert-row (if after (1+ row) row))
             (old-len (length lines))
             (new-len (+ old-len num-new)))
        (adjust-array lines new-len :fill-pointer new-len)
        (loop for i from (1- new-len) downto (+ insert-row num-new) do
          (setf (aref lines i) (aref lines (- i num-new))))
        (loop for line in clip-lines
              for idx from insert-row do
                (setf (aref lines idx) line))
        (setf (editor-buffer-cursor-row buf) insert-row
              (editor-buffer-cursor-col buf) 0
              (buffer-modified-p buf) t))
      (%clamp-cursor buf))))

(defun %find-char (buf target-char &key (forward t) (till nil))
  "Find TARGET-CHAR in current line and position cursor."
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf))
         (len (length cur)))
    (if forward
        (let ((pos (position target-char cur :start (min len (1+ col)))))
          (when pos
            (setf (editor-buffer-cursor-col buf)
                  (if till (max col (1- pos)) pos))))
        (let ((pos (position target-char cur :end (max 0 col) :from-end t)))
          (when pos
            (setf (editor-buffer-cursor-col buf)
                  (if till (min col (1+ pos)) pos)))))
    (%clamp-cursor buf)))

(defun %word-char-p (c)
  (and c (or (alphanumericp c) (char= c #\_) (char= c #\-))))

(defun %move-next-word (buf)
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf))
         (len (length cur)))
    (cond
      ((< col len)
       (let ((i col))
         (loop while (and (< i len) (%word-char-p (char cur i))) do (incf i))
         (loop while (and (< i len) (not (%word-char-p (char cur i)))) do (incf i))
         (if (< i len)
             (setf (editor-buffer-cursor-col buf) i)
             (if (< (1+ (editor-buffer-cursor-row buf)) (length (editor-buffer-lines buf)))
                 (progn
                   (incf (editor-buffer-cursor-row buf))
                   (setf (editor-buffer-cursor-col buf) 0))
                 (setf (editor-buffer-cursor-col buf) (max 0 (1- len)))))))
      (t
       (when (< (1+ (editor-buffer-cursor-row buf)) (length (editor-buffer-lines buf)))
         (incf (editor-buffer-cursor-row buf))
         (setf (editor-buffer-cursor-col buf) 0))))))

(defun %move-prev-word (buf)
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf)))
    (cond
      ((> col 0)
       (let ((i (1- col)))
         (loop while (and (> i 0) (not (%word-char-p (char cur i)))) do (decf i))
         (loop while (and (> i 0) (%word-char-p (char cur (1- i)))) do (decf i))
         (setf (editor-buffer-cursor-col buf) i)))
      (t
       (when (> (editor-buffer-cursor-row buf) 0)
         (decf (editor-buffer-cursor-row buf))
         (let ((prev-len (length (%current-line buf))))
           (setf (editor-buffer-cursor-col buf) (max 0 (1- prev-len)))))))))

;;; ---------------------------------------------------------------------------
;;; Helix Command Execution (:w, :q, :wq, :e, :scratch)
;;; ---------------------------------------------------------------------------

(defun %execute-helix-command (buf cmd-str)
  (let* ((trimmed (string-trim '(#\Space #\Tab) cmd-str))
         (space-pos (position #\Space trimmed))
         (verb (if space-pos (subseq trimmed 0 space-pos) trimmed))
         (arg (if space-pos (string-trim '(#\Space #\Tab) (subseq trimmed (1+ space-pos))) nil)))
    (cond
      ;; :w or :write
      ((or (string= verb "w") (string= verb "write"))
       (if (and arg (> (length arg) 0))
           (save-buffer buf arg)
           (save-buffer buf)))

      ;; :q or :quit
      ((or (string= verb "q") (string= verb "quit"))
       (if (and (buffer-modified-p buf) (not (typep buf 'scratch-buffer)))
           (setf (editor-buffer-status-message buf) "[Unsaved changes! Use :q! to force quit]")
           (kill-buffer buf)))

      ;; :q! (force quit)
      ((string= verb "q!")
       (kill-buffer buf))

      ;; :wq or :x
      ((or (string= verb "wq") (string= verb "x"))
       (save-buffer buf)
       (kill-buffer buf))

      ;; :e or :edit
      ((or (string= verb "e") (string= verb "edit"))
       (if (and arg (> (length arg) 0))
           (let ((new-buf (open-file-in-buffer arg)))
             (when (and new-buf (find-package :lispos.window))
               (let ((focus-sym (find-symbol "*FOCUSED-WINDOW*" :lispos.window)))
                 (when (and focus-sym (symbol-value focus-sym))
                   (setf (slot-value (symbol-value focus-sym) (find-symbol "BUFFER" :lispos.window)) new-buf
                         *current-buffer* new-buf)))))
           (setf (editor-buffer-status-message buf) "Usage: :e <filename>")))

      ;; :scratch
      ((string= verb "scratch")
       (let ((sbuf (create-scratch-buffer)))
         (when (and sbuf (find-package :lispos.window))
           (let ((focus-sym (find-symbol "*FOCUSED-WINDOW*" :lispos.window)))
             (when (and focus-sym (symbol-value focus-sym))
               (setf (slot-value (symbol-value focus-sym) (find-symbol "BUFFER" :lispos.window)) sbuf
                     *current-buffer* sbuf))))))

      (t
       (setf (editor-buffer-status-message buf)
             (format nil "Unknown command: :~A" verb))))))

;;; ---------------------------------------------------------------------------
;;; Helix Space Leader Actions (SPC f, SPC b, SPC w, SPC q, SPC s)
;;; ---------------------------------------------------------------------------

(defun %handle-leader-key (buf key char)
  (setf (editor-buffer-leader-active-p buf) nil)
  (let ((k (if (characterp (or char key)) (char-downcase (or char key)) key)))
    (case k
      ;; SPC f: Open file picker
      ((#\f :f)
       (setf (editor-buffer-status-message buf) nil)
       (when (fboundp (find-symbol "OPEN-FILE-PICKER" :lispos.picker))
         (funcall (find-symbol "OPEN-FILE-PICKER" :lispos.picker)
                  (lambda (file-path)
                    (when file-path
                      (let ((new-buf (open-file-in-buffer file-path)))
                        (when (find-package :lispos.window)
                          (let ((fwin (symbol-value (find-symbol "*FOCUSED-WINDOW*" :lispos.window))))
                            (when fwin
                              (setf (slot-value fwin (find-symbol "BUFFER" :lispos.window)) new-buf
                                    *current-buffer* new-buf))))))))))

      ;; SPC b: Open buffer picker
      ((#\b :b)
       (setf (editor-buffer-status-message buf) nil)
       (when (fboundp (find-symbol "OPEN-BUFFER-PICKER" :lispos.picker))
         (funcall (find-symbol "OPEN-BUFFER-PICKER" :lispos.picker)
                  (lambda (picked-buf)
                    (when picked-buf
                      (when (find-package :lispos.window)
                        (let ((fwin (symbol-value (find-symbol "*FOCUSED-WINDOW*" :lispos.window))))
                          (when fwin
                            (setf (slot-value fwin (find-symbol "BUFFER" :lispos.window)) picked-buf
                                  *current-buffer* picked-buf)))))))))

      ;; SPC w: Save buffer
      ((#\w :w)
       (save-buffer buf))

      ;; SPC q: Quit buffer
      ((#\q :q)
       (if (and (buffer-modified-p buf) (not (typep buf 'scratch-buffer)))
           (setf (editor-buffer-status-message buf) "[Unsaved changes! Use :q! to force quit]")
           (kill-buffer buf)))

      ;; SPC s: Open or switch to *scratch*
      ((#\s :s)
       (let ((sbuf (create-scratch-buffer)))
         (when (find-package :lispos.window)
           (let ((fwin (symbol-value (find-symbol "*FOCUSED-WINDOW*" :lispos.window))))
             (when fwin
               (setf (slot-value fwin (find-symbol "BUFFER" :lispos.window)) sbuf
                     *current-buffer* sbuf))))))

      ;; Cancel on Escape / Space
      ((:escape #\Esc #\Space :space)
       (setf (editor-buffer-status-message buf) "[Cancelled]"))

      (t
       (setf (editor-buffer-status-message buf) (format nil "[Unknown leader: ~A]" k))))))

;;; ---------------------------------------------------------------------------
;;; Helix Modal Input Dispatcher
;;; ---------------------------------------------------------------------------

(defmethod buffer-handle-key ((buf editor-buffer) key &key (char nil) (modifiers nil))
  (declare (ignore modifiers))
  ;; Clear transient status message on new keypress unless leader or pending
  (unless (or (editor-buffer-leader-active-p buf)
              (editor-buffer-pending-command buf))
    (setf (editor-buffer-status-message buf) nil))

  ;; 1. Check if Space leader key is active awaiting secondary key
  (when (editor-buffer-leader-active-p buf)
    (%handle-leader-key buf key char)
    (return-from buffer-handle-key t))

  ;; 2. Check if a pending multi-key sequence is active
  (when (editor-buffer-pending-command buf)
    (let ((cmd (editor-buffer-pending-command buf))
          (k (if (characterp (or char key)) (or char key) key)))
      (setf (editor-buffer-pending-command buf) nil)
      (case cmd
        (:go
         (cond
           ((eq k #\g)
            (setf (editor-buffer-cursor-row buf) 0
                  (editor-buffer-cursor-col buf) 0))
           ((eq k #\e)
            (setf (editor-buffer-cursor-row buf) (max 0 (1- (length (editor-buffer-lines buf))))
                  (editor-buffer-cursor-col buf) 0))
           ((eq k #\h)
            (setf (editor-buffer-cursor-col buf) 0))
           ((eq k #\l)
            (setf (editor-buffer-cursor-col buf) (max 0 (1- (length (%current-line buf)))))))
         (%clamp-cursor buf)
         (return-from buffer-handle-key t))

        (:find-char-fwd
         (when (characterp k)
           (%find-char buf k :forward t :till nil))
         (return-from buffer-handle-key t))

        (:till-char-fwd
         (when (characterp k)
           (%find-char buf k :forward t :till t))
         (return-from buffer-handle-key t))

        (:find-char-bwd
         (when (characterp k)
           (%find-char buf k :forward nil :till nil))
         (return-from buffer-handle-key t))

        (:till-char-bwd
         (when (characterp k)
           (%find-char buf k :forward nil :till t))
         (return-from buffer-handle-key t))

        (:replace-char
         (when (and (characterp k) (graphic-char-p k))
           (%push-undo-snapshot buf)
           (if (eq (editor-buffer-helix-mode buf) :select)
               (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
                 (let ((lines (editor-buffer-lines buf)))
                   (loop for r from s-row to e-row do
                     (let* ((line (aref lines r))
                            (c-start (if (= r s-row) s-col 0))
                            (c-end (if (= r e-row) (min (1+ e-col) (length line)) (length line)))
                            (rep-count (max 0 (- c-end c-start)))
                            (rep-str (make-string rep-count :initial-element k)))
                       (setf (aref lines r)
                             (concatenate 'string (subseq line 0 c-start) rep-str (subseq line c-end))))))
                 (setf (editor-buffer-helix-mode buf) :normal
                       (editor-buffer-selection-anchor-row buf) nil
                       (editor-buffer-selection-anchor-col buf) nil
                       (buffer-modified-p buf) t))
               (let* ((cur (%current-line buf))
                      (col (editor-buffer-cursor-col buf)))
                 (when (< col (length cur))
                   (setf (aref cur col) k
                         (buffer-modified-p buf) t))))
           (%clamp-cursor buf))
         (return-from buffer-handle-key t)))))

  ;; 3. Route according to active Helix mode
  (case (editor-buffer-helix-mode buf)
    ;; ------------------------------------------------------------------------
    ;; Mode 1: Normal Mode
    ;; ------------------------------------------------------------------------
    (:normal
     (let ((k (if (characterp (or char key)) (or char key) key)))
       (cond
         ;; Space Leader Key
         ((or (eq k #\Space) (eq k :space))
          (setf (editor-buffer-leader-active-p buf) t
                (editor-buffer-status-message buf) "[SPC] f: file  b: buffer  w: save  q: quit  s: scratch"))

         ;; Motions: h, j, k, l / Arrows
         ((or (eq k #\h) (eq k :left))
          (decf (editor-buffer-cursor-col buf))
          (%clamp-cursor buf))
         ((or (eq k #\l) (eq k :right))
          (incf (editor-buffer-cursor-col buf))
          (%clamp-cursor buf))
         ((or (eq k #\j) (eq k :down))
          (incf (editor-buffer-cursor-row buf))
          (%clamp-cursor buf))
         ((or (eq k #\k) (eq k :up))
          (decf (editor-buffer-cursor-row buf))
          (%clamp-cursor buf))

         ;; Words: w, b, e
         ((eq k #\w) (%move-next-word buf))
         ((eq k #\b) (%move-prev-word buf))
         ((eq k #\e)
          (%move-next-word buf)
          (%clamp-cursor buf))

         ;; Line Navigation: 0, $
         ((or (eq k #\0) (eq k :home))
          (setf (editor-buffer-cursor-col buf) 0))
         ((or (eq k #\$) (eq k :end))
          (setf (editor-buffer-cursor-col buf) (max 0 (1- (length (%current-line buf))))))

         ;; Go Motions: g, G
         ((eq k #\g)
          (if (eq (editor-buffer-last-key buf) #\g)
              (progn
                (setf (editor-buffer-cursor-row buf) 0
                      (editor-buffer-cursor-col buf) 0
                      (editor-buffer-last-key buf) nil))
              (setf (editor-buffer-pending-command buf) :go
                    (editor-buffer-status-message buf) "[g] g: top  e: end  h: start  l: end")))
         ((eq k #\G)
          (setf (editor-buffer-cursor-row buf) (max 0 (1- (length (editor-buffer-lines buf))))
                (editor-buffer-cursor-col buf) 0))

         ;; Char search: f, t, F, T
         ((eq k #\f)
          (setf (editor-buffer-pending-command buf) :find-char-fwd
                (editor-buffer-status-message buf) "[f: char forward]"))
         ((eq k #\t)
          (setf (editor-buffer-pending-command buf) :till-char-fwd
                (editor-buffer-status-message buf) "[t: char till forward]"))
         ((eq k #\F)
          (setf (editor-buffer-pending-command buf) :find-char-bwd
                (editor-buffer-status-message buf) "[F: char backward]"))
         ((eq k #\T)
          (setf (editor-buffer-pending-command buf) :till-char-bwd
                (editor-buffer-status-message buf) "[T: char till backward]"))

         ;; Visual Selection Mode: v, V
         ((eq k #\v)
          (setf (editor-buffer-helix-mode buf) :select
                (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)
                (editor-buffer-selection-anchor-col buf) (editor-buffer-cursor-col buf)
                (editor-buffer-status-message buf) "[Select mode]"))
         ((eq k #\V)
          (let ((line-len (length (%current-line buf))))
            (setf (editor-buffer-helix-mode buf) :select
                  (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)
                  (editor-buffer-selection-anchor-col buf) 0
                  (editor-buffer-cursor-col buf) (max 0 (1- line-len))
                  (editor-buffer-status-message buf) "[Select line]")))

         ;; Undo / Redo: u, U
         ((eq k #\u)
          (%undo buf))
         ((eq k #\U)
          (%redo buf))

         ;; Enter Insert Mode: i, a, o, O
         ((eq k #\i)
          (%push-undo-snapshot buf)
          (setf (editor-buffer-helix-mode buf) :insert))
         ((eq k #\a)
          (%push-undo-snapshot buf)
          (let ((cur-len (length (%current-line buf))))
            (when (< (editor-buffer-cursor-col buf) cur-len)
              (incf (editor-buffer-cursor-col buf))))
          (setf (editor-buffer-helix-mode buf) :insert))
         ((eq k #\o)
          (%push-undo-snapshot buf)
          (let* ((lines (editor-buffer-lines buf))
                 (row (editor-buffer-cursor-row buf))
                 (new-len (1+ (length lines))))
            (vector-push-extend "" lines)
            (loop for i from (1- new-len) downto (+ row 2) do
              (setf (aref lines i) (aref lines (1- i))))
            (setf (aref lines (1+ row)) "")
            (incf (editor-buffer-cursor-row buf))
            (setf (editor-buffer-cursor-col buf) 0)
            (setf (editor-buffer-helix-mode buf) :insert)
            (setf (buffer-modified-p buf) t)))
         ((eq k #\O)
          (%push-undo-snapshot buf)
          (let* ((lines (editor-buffer-lines buf))
                 (row (editor-buffer-cursor-row buf))
                 (new-len (1+ (length lines))))
            (vector-push-extend "" lines)
            (loop for i from (1- new-len) downto (1+ row) do
              (setf (aref lines i) (aref lines (1- i))))
            (setf (aref lines row) "")
            (setf (editor-buffer-cursor-col buf) 0)
            (setf (editor-buffer-helix-mode buf) :insert)
            (setf (buffer-modified-p buf) t)))

         ;; Delete / Change: d, x, c, r
         ((or (eq k #\x) (eq k :delete))
          (%delete-char-under-point buf)
          (setf (editor-buffer-last-key buf) #\x))
         ((eq k #\d)
          (if (or (eq (editor-buffer-last-key buf) #\d)
                  (eq (editor-buffer-last-key buf) #\x))
              (progn
                (setf (editor-buffer-clipboard buf) (%current-line buf))
                (%delete-current-line buf)
                (setf (editor-buffer-last-key buf) nil))
              (setf (editor-buffer-last-key buf) #\d)))
         ((eq k #\c)
          (%push-undo-snapshot buf)
          (%delete-char-under-point buf)
          (setf (editor-buffer-helix-mode buf) :insert))
         ((eq k #\r)
          (setf (editor-buffer-pending-command buf) :replace-char
                (editor-buffer-status-message buf) "[r: char to replace]"))

         ;; Yank / Paste: y, p, P
         ((eq k #\y)
          (setf (editor-buffer-clipboard buf) (%current-line buf))
          (setf (editor-buffer-status-message buf) "[Yanked 1 line]"))
         ((eq k #\p)
          (%paste buf :after t))
         ((eq k #\P)
          (%paste buf :after nil))

         ;; Command Mode: :
         ((eq k #\:)
          (setf (editor-buffer-helix-mode buf) :command
                (editor-buffer-command-input buf) ""))

         (t nil))))

    ;; ------------------------------------------------------------------------
    ;; Mode 2: Visual Selection Mode (:select)
    ;; ------------------------------------------------------------------------
    (:select
     (let ((k (if (characterp (or char key)) (or char key) key)))
       (cond
         ;; Escape or 'v' cancels selection and returns to normal mode
         ((or (eq key :escape) (eq char #\Esc) (eq k #\v))
          (setf (editor-buffer-helix-mode buf) :normal
                (editor-buffer-selection-anchor-row buf) nil
                (editor-buffer-selection-anchor-col buf) nil)
          (%clamp-cursor buf))

         ;; Space Leader in selection mode
         ((or (eq k #\Space) (eq k :space))
          (setf (editor-buffer-leader-active-p buf) t
                (editor-buffer-status-message buf) "[SPC] f: file  b: buffer  w: save  q: quit  s: scratch"))

         ;; Line selection extend: x (down), X (up)
         ((eq k #\x)
          (let* ((lines (editor-buffer-lines buf))
                 (next-r (min (1- (length lines)) (1+ (editor-buffer-cursor-row buf))))
                 (next-line (aref lines next-r)))
            (setf (editor-buffer-cursor-row buf) next-r
                  (editor-buffer-cursor-col buf) (max 0 (1- (length next-line))))))
         ((eq k #\X)
          (let* ((lines (editor-buffer-lines buf))
                 (prev-r (max 0 (1- (editor-buffer-cursor-row buf))))
                 (prev-line (aref lines prev-r)))
            (setf (editor-buffer-cursor-row buf) prev-r
                  (editor-buffer-cursor-col buf) (max 0 (1- (length prev-line))))))

         ;; Motions extend selection
         ((or (eq k #\h) (eq k :left))
          (decf (editor-buffer-cursor-col buf)) (%clamp-cursor buf))
         ((or (eq k #\l) (eq k :right))
          (incf (editor-buffer-cursor-col buf)) (%clamp-cursor buf))
         ((or (eq k #\j) (eq k :down))
          (incf (editor-buffer-cursor-row buf)) (%clamp-cursor buf))
         ((or (eq k #\k) (eq k :up))
          (decf (editor-buffer-cursor-row buf)) (%clamp-cursor buf))
         ((eq k #\w) (%move-next-word buf))
         ((eq k #\b) (%move-prev-word buf))
         ((eq k #\e) (%move-next-word buf) (%clamp-cursor buf))
         ((or (eq k #\0) (eq k :home)) (setf (editor-buffer-cursor-col buf) 0))
         ((or (eq k #\$) (eq k :end)) (setf (editor-buffer-cursor-col buf) (max 0 (1- (length (%current-line buf))))))
         ((eq k #\G) (setf (editor-buffer-cursor-row buf) (max 0 (1- (length (editor-buffer-lines buf)))) (editor-buffer-cursor-col buf) 0))

         ;; Selection Actions: d (delete), c (change), y (yank), r (replace)
         ((or (eq k #\d) (eq k :delete))
          (%delete-selection buf))
         ((eq k #\c)
          (%delete-selection buf)
          (setf (editor-buffer-helix-mode buf) :insert))
         ((eq k #\y)
          (setf (editor-buffer-clipboard buf) (%extract-selection-text buf)
                (editor-buffer-helix-mode buf) :normal
                (editor-buffer-selection-anchor-row buf) nil
                (editor-buffer-selection-anchor-col buf) nil
                (editor-buffer-status-message buf) "[Yanked selection]"))
         ((eq k #\r)
          (setf (editor-buffer-pending-command buf) :replace-char
                (editor-buffer-status-message buf) "[r: char to replace]"))

         (t nil))))

    ;; ------------------------------------------------------------------------
    ;; Mode 3: Insert Mode
    ;; ------------------------------------------------------------------------
    (:insert
     (cond
       ;; Escape returns to Normal mode
       ((or (eq key :escape) (eq char #\Esc))
        (setf (editor-buffer-helix-mode buf) :normal)
        (%clamp-cursor buf))

       ;; Enter / Return splits line
       ((or (eq key :enter) (eq key :return) (eq char #\Newline) (eq char #\Return))
        (%split-line-at-point buf))

       ;; Backspace
       ((or (eq key :backspace) (eq char #\Backspace) (eq char #\Rubout))
        (%delete-char-before-point buf))

       ;; Delete
       ((or (eq key :delete) (eq char #\Delete))
        (%delete-char-under-point buf))

       ;; Navigation inside Insert mode
       ((eq key :left)  (decf (editor-buffer-cursor-col buf)) (%clamp-cursor buf))
       ((eq key :right) (incf (editor-buffer-cursor-col buf)) (%clamp-cursor buf))
       ((eq key :up)    (decf (editor-buffer-cursor-row buf)) (%clamp-cursor buf))
       ((eq key :down)  (incf (editor-buffer-cursor-row buf)) (%clamp-cursor buf))

       ;; Printable character insertion
       ((characterp (or char key))
        (let ((c (or char key)))
          (when (graphic-char-p c)
            (%insert-char-at-point buf c))))
       (t nil)))

    ;; ------------------------------------------------------------------------
    ;; Mode 4: Command Mode (":w", ":q", etc.)
    ;; ------------------------------------------------------------------------
    (:command
     (cond
       ((or (eq key :escape) (eq char #\Esc))
        (setf (editor-buffer-helix-mode buf) :normal
              (editor-buffer-command-input buf) ""))

       ((or (eq key :enter) (eq key :return) (eq char #\Newline) (eq char #\Return))
        (let ((cmd (editor-buffer-command-input buf)))
          (setf (editor-buffer-helix-mode buf) :normal
                (editor-buffer-command-input buf) "")
          (%execute-helix-command buf cmd)))

       ((or (eq key :backspace) (eq char #\Backspace) (eq char #\Rubout))
        (let ((inp (editor-buffer-command-input buf)))
          (if (> (length inp) 0)
              (setf (editor-buffer-command-input buf) (subseq inp 0 (1- (length inp))))
              (setf (editor-buffer-helix-mode buf) :normal))))

       ((characterp (or char key))
        (let ((c (or char key)))
          (when (graphic-char-p c)
            (setf (editor-buffer-command-input buf)
                  (concatenate 'string (editor-buffer-command-input buf) (string c))))))
       (t nil)))))

;;; ---------------------------------------------------------------------------
;;; Concrete Subclasses: FileBuffer & ScratchBuffer
;;; ---------------------------------------------------------------------------

(defclass file-buffer (editor-buffer)
  ((file-path
    :initarg :file-path
    :initform nil
    :accessor file-buffer-path
    :type (or null string)
    :documentation "Absolute path of backing file on disk."))
  (:documentation "Editor buffer with internal file persistence logic."))

(defmethod buffer-file-path ((buf file-buffer))
  (file-buffer-path buf))

(defmethod buffer-mode ((buf file-buffer))
  :file)

(defmethod initialize-instance :after ((buf file-buffer) &key &allow-other-keys)
  (let ((path (file-buffer-path buf)))
    (if (and path (probe-file path))
        (handler-case
            (with-open-file (stream path :direction :input :if-does-not-exist nil)
              (when stream
                (let ((lines (make-array 16 :adjustable t :fill-pointer 0)))
                  (loop for line = (read-line stream nil nil)
                        while line do
                          (vector-push-extend line lines))
                  (when (zerop (length lines))
                    (vector-push-extend "" lines))
                  (setf (editor-buffer-lines buf) lines
                        (buffer-modified-p buf) nil))))
          (error (c)
            (setf (editor-buffer-status-message buf) (format nil "Load error: ~A" c))))
        (when path
          (setf (editor-buffer-status-message buf) (format nil "[New file: ~A]" (file-namestring path)))))))

(defmethod save-buffer ((buf file-buffer) &optional path)
  (let ((target (or path (file-buffer-path buf))))
    (if (null target)
        (setf (editor-buffer-status-message buf) "Error: No file path specified. Use :w <path>")
        (handler-case
            (progn
              (ensure-directories-exist target)
              (with-open-file (stream target
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (let ((lines (editor-buffer-lines buf)))
                  (loop for i from 0 below (length lines) do
                    (write-line (aref lines i) stream))))
              (setf (file-buffer-path buf) target
                    (buffer-name buf) (file-namestring target)
                    (buffer-modified-p buf) nil
                    (editor-buffer-status-message buf)
                    (format nil "[Wrote ~D lines to ~A]" (length (editor-buffer-lines buf)) target)))
          (error (c)
            (setf (editor-buffer-status-message buf) (format nil "Save error: ~A" c)))))))

;; Scratch buffer subclass
(defclass scratch-buffer (editor-buffer)
  ()
  (:default-initargs
   :name "*scratch*"
   :lines (make-array 5 :adjustable t :fill-pointer 5
                        :initial-contents
                        (list ";; *scratch* - LispOS Interactive Scratchpad (Helix mode)"
                              ";; Press Space w or :w <file> to save this buffer to disk."
                              ";; Press Space b to switch buffers, Space f to find files."
                              ""
                              "(+ 40 2)")))
  (:documentation "In-memory scratchpad buffer with Helix keybindings."))

(defmethod save-buffer ((buf scratch-buffer) &optional path)
  (if path
      (handler-case
          (progn
            (ensure-directories-exist path)
            (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
              (let ((lines (editor-buffer-lines buf)))
                (loop for i from 0 below (length lines) do
                  (write-line (aref lines i) stream))))
            (change-class buf 'file-buffer :file-path path)
            (setf (buffer-name buf) (file-namestring path)
                  (buffer-modified-p buf) nil
                  (editor-buffer-status-message buf) (format nil "[Saved as ~A]" path)))
        (error (c)
          (setf (editor-buffer-status-message buf) (format nil "Save error: ~A" c))))
      (setf (editor-buffer-status-message buf) "Use :w <path> to save *scratch* to a file.")))

;;; ---------------------------------------------------------------------------
;;; Buffer Helper Constructors
;;; ---------------------------------------------------------------------------

(defun open-file-in-buffer (path)
  "Open disk file PATH into a file-buffer and register it."
  (let* ((truename-str (or (ignore-errors (namestring (truename path))) path))
         (existing (find-if (lambda (b)
                              (and (typep b 'file-buffer)
                                   (file-buffer-path b)
                                   (string= (file-buffer-path b) truename-str)))
                            *buffers*)))
    (if existing
        existing
        (let* ((base-name (file-namestring path))
               (name (if (and base-name (> (length base-name) 0)) base-name path))
               (unique-name (if (find-buffer name)
                                (format nil "~A<~A>" name (or (car (last (pathname-directory (pathname path)))) "1"))
                                name))
               (buf (make-instance 'file-buffer :name unique-name :file-path path)))
          (register-buffer buf)))))

(defun create-scratch-buffer (&optional (name "*scratch*"))
  "Create and register a *scratch* editor buffer."
  (let ((existing (find-buffer name)))
    (if existing
        existing
        (let ((buf (make-instance 'scratch-buffer :name name)))
          (register-buffer buf)))))
