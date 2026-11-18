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
   (selection-end-row
    :initform nil
    :accessor editor-buffer-selection-end-row
    :type (or null fixnum)
    :documentation "Row where visual selection ends (if head-exclusive/explicit), or NIL.")
   (selection-end-col
    :initform nil
    :accessor editor-buffer-selection-end-col
    :type (or null fixnum)
    :documentation "Col where visual selection ends (if head-exclusive/explicit), or NIL.")
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

(defmethod buffer-mode-icon ((buf editor-buffer))
  (case (editor-buffer-helix-mode buf)
    (:insert  (code-char #xF448))    ; pencil 
    (:select  (code-char #xF246))    ; I-beam 
    (t        (code-char #xF09A8)))) ; text document 󰦨

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
        (er (or (editor-buffer-selection-end-row buf) (editor-buffer-cursor-row buf)))
        (ec (or (editor-buffer-selection-end-col buf) (editor-buffer-cursor-col buf))))
    (if (or (< ar er) (and (= ar er) (<= ac ec)))
        (values ar ac er ec)
        (values er ec ar ac))))

(defun %extract-selection-text (buf)
  "Extract text in selection bounds as a string."
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (let ((lines (editor-buffer-lines buf)))
      (cond
        ((= s-row e-row)
         (let* ((line (aref lines s-row))
                (c1 (min s-col (length line)))
                (c2 (min (1+ e-col) (length line))))
           (if (< c1 c2) (subseq line c1 c2) "")))
        (t
         (with-output-to-string (s)
           (let* ((first-line (aref lines s-row))
                  (c1 (min s-col (length first-line))))
             (write-line (subseq first-line c1) s))
           (loop for r from (1+ s-row) below e-row do
             (write-line (aref lines r) s))
           (let* ((last-line (aref lines e-row))
                  (c2 (min (1+ e-col) (length last-line))))
             (write-string (subseq last-line 0 c2) s))))))))

(defun %delete-selection (buf)
  "Helix `d`: Delete text in selection bounds (or char under cursor if no anchor)."
  (%push-undo-snapshot buf)
  ;; If no multi-char selection anchor, delete single char under point
  (unless (and (editor-buffer-selection-anchor-row buf)
               (editor-buffer-selection-anchor-col buf))
    (let* ((cur (%current-line buf))
           (col (editor-buffer-cursor-col buf)))
      (if (< col (length cur))
          (setf (editor-buffer-clipboard buf) (string (char cur col)))
          (setf (editor-buffer-clipboard buf) (string #\Newline)))
      (%delete-char-under-point buf)
      (return-from %delete-selection t)))

  (let ((clip (%extract-selection-text buf)))
    (setf (editor-buffer-clipboard buf) clip))
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (let ((lines (editor-buffer-lines buf)))
      (cond
        ((= s-row e-row)
         (let ((line (aref lines s-row)))
           (if (and (= s-col 0) (>= e-col (max 0 (1- (length line)))))
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
                      (c2 (min (1+ e-col) (length line)))
                      (new-line (concatenate 'string (subseq line 0 c1) (subseq line c2))))
                 (setf (aref lines s-row) new-line
                       (editor-buffer-cursor-row buf) s-row
                       (editor-buffer-cursor-col buf) c1)))))
        (t
         (let* ((first-line (aref lines s-row))
                (last-line (aref lines e-row))
                (full-multi-p (and (= s-col 0) (>= e-col (max 0 (1- (length last-line)))))))
           (if full-multi-p
               ;; All lines from s-row to e-row are fully selected
               (let* ((num-deleted (1+ (- e-row s-row)))
                      (len (length lines)))
                 (if (= num-deleted len)
                     (progn
                       (setf (fill-pointer lines) 1)
                       (setf (aref lines 0) "")
                       (setf (editor-buffer-cursor-row buf) 0
                             (editor-buffer-cursor-col buf) 0))
                     (progn
                       (loop for i from s-row below (- len num-deleted) do
                         (setf (aref lines i) (aref lines (+ i num-deleted))))
                       (decf (fill-pointer lines) num-deleted)
                       (setf (editor-buffer-cursor-row buf) (max 0 (min s-row (1- (length lines))))
                             (editor-buffer-cursor-col buf) 0))))
               ;; Partial multi-line deletion
               (let* ((c1 (min s-col (length first-line)))
                      (c2 (min (1+ e-col) (length last-line)))
                      (merged-line (concatenate 'string (subseq first-line 0 c1) (subseq last-line c2)))
                      (deleted-count (- e-row s-row))
                      (len (length lines)))
                 (setf (aref lines s-row) merged-line)
                 (loop for i from (1+ s-row) below (- len deleted-count) do
                   (setf (aref lines i) (aref lines (+ i deleted-count))))
                 (decf (fill-pointer lines) deleted-count)
                 (setf (editor-buffer-cursor-row buf) s-row
                       (editor-buffer-cursor-col buf) c1))))))))
  (setf (editor-buffer-helix-mode buf) :normal
        (editor-buffer-selection-anchor-row buf) nil
        (editor-buffer-selection-anchor-col buf) nil
        (editor-buffer-selection-end-row buf) nil
        (editor-buffer-selection-end-col buf) nil
        (buffer-modified-p buf) t)
  (%clamp-cursor buf))

(defun %change-selection (buf)
  "Helix `c`: Delete selection, yank to clipboard, and enter insert mode."
  (%delete-selection buf)
  (setf (editor-buffer-helix-mode buf) :insert))

(defun %yank-selection (buf)
  "Helix `y`: Yank selection to clipboard, collapse selection, and return to normal mode."
  (let ((clip (%extract-selection-text buf)))
    (if (> (length clip) 0)
        (setf (editor-buffer-clipboard buf) clip)
        (let* ((cur (%current-line buf))
               (col (editor-buffer-cursor-col buf)))
          (setf (editor-buffer-clipboard buf)
                (if (< col (length cur)) (string (char cur col)) ""))))
    (setf (editor-buffer-helix-mode buf) :normal
          (editor-buffer-selection-anchor-row buf) nil
          (editor-buffer-selection-anchor-col buf) nil
          (editor-buffer-selection-end-row buf) nil
          (editor-buffer-selection-end-col buf) nil
          (editor-buffer-status-message buf) "[Yanked selection]")))

(defun %collapse-selection (buf)
  "Helix `;`: Collapse selection onto a single cursor point."
  (setf (editor-buffer-selection-anchor-row buf) nil
        (editor-buffer-selection-anchor-col buf) nil
        (editor-buffer-selection-end-row buf) nil
        (editor-buffer-selection-end-col buf) nil
        (editor-buffer-helix-mode buf) :normal
        (editor-buffer-status-message buf) "[Collapsed selection]"))

(defun %flip-selections (buf)
  "Helix `Alt-;`: Swap cursor head and selection anchor."
  (when (and (editor-buffer-selection-anchor-row buf)
             (editor-buffer-selection-anchor-col buf))
    (let ((ar (editor-buffer-selection-anchor-row buf))
          (ac (editor-buffer-selection-anchor-col buf))
          (cr (editor-buffer-cursor-row buf))
          (cc (editor-buffer-cursor-col buf)))
      (setf (editor-buffer-cursor-row buf) ar
            (editor-buffer-cursor-col buf) ac
            (editor-buffer-selection-anchor-row buf) cr
            (editor-buffer-selection-anchor-col buf) cc
            (editor-buffer-selection-end-row buf) nil
            (editor-buffer-selection-end-col buf) nil)
      (%clamp-cursor buf)
      (setf (editor-buffer-status-message buf) "[Flipped selection]"))))

(defun %select-line-below (buf)
  "Helix `x`: Select current line, or extend line selection to next line."
  (let* ((lines (editor-buffer-lines buf))
         (row (editor-buffer-cursor-row buf))
         (cur-len (length (%current-line buf))))
    (setf (editor-buffer-selection-end-row buf) nil
          (editor-buffer-selection-end-col buf) nil)
    (if (and (editor-buffer-selection-anchor-row buf)
             (= (or (editor-buffer-selection-anchor-col buf) 0) 0))
        ;; Already in line selection: extend downward
        (let* ((next-r (min (1- (length lines)) (1+ row)))
               (next-line (aref lines next-r)))
          (setf (editor-buffer-cursor-row buf) next-r
                (editor-buffer-cursor-col buf) (max 0 (1- (length next-line))))
          (setf (editor-buffer-status-message buf)
                (format nil "[Selected ~D lines]"
                        (1+ (abs (- next-r (editor-buffer-selection-anchor-row buf)))))))
        ;; Select current line
        (progn
          (setf (editor-buffer-selection-anchor-row buf) row
                (editor-buffer-selection-anchor-col buf) 0
                (editor-buffer-cursor-col buf) (max 0 (1- cur-len)))
          (setf (editor-buffer-status-message buf) "[Selected 1 line]")))))

(defun %select-line-bounds (buf)
  "Helix `X`: Extend current selection to line bounds."
  (setf (editor-buffer-selection-anchor-col buf) 0
        (editor-buffer-cursor-col buf) (max 0 (1- (length (%current-line buf))))
        (editor-buffer-status-message buf) "[Selected line bounds]"))

(defun %select-all (buf)
  "Helix `%`: Select entire buffer."
  (let* ((lines (editor-buffer-lines buf))
         (last-r (max 0 (1- (length lines))))
         (last-line (aref lines last-r)))
    (setf (editor-buffer-selection-anchor-row buf) 0
          (editor-buffer-selection-anchor-col buf) 0
          (editor-buffer-cursor-row buf) last-r
          (editor-buffer-cursor-col buf) (max 0 (1- (length last-line)))
          (editor-buffer-status-message buf) "[Selected all]")))

(defun %replace-selection-with-yanked (buf)
  "Helix `R`: Replace current selection with clipboard content."
  (let ((clip (editor-buffer-clipboard buf)))
    (when (> (length clip) 0)
      (%delete-selection buf)
      (%paste buf :after nil))))

(defun %switch-case (buf)
  "Helix `~`: Toggle case of all characters in current selection."
  (%push-undo-snapshot buf)
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (let ((lines (editor-buffer-lines buf)))
      (loop for r from s-row to e-row do
        (let* ((line (copy-seq (aref lines r)))
               (len (length line))
               (c-start (if (= r s-row) (min s-col len) 0))
               (c-end (if (= r e-row) (min (1+ e-col) len) len)))
          (loop for c-idx from c-start below c-end do
            (let ((ch (char line c-idx)))
              (setf (char line c-idx)
                    (if (upper-case-p ch)
                        (char-downcase ch)
                        (char-upcase ch)))))
          (setf (aref lines r) line)))))
  (setf (buffer-modified-p buf) t)
  (setf (editor-buffer-status-message buf) "[Switched case]"))

(defun %switch-to-lowercase (buf)
  "Helix ``` ` ```: Convert characters in selection to lowercase."
  (%push-undo-snapshot buf)
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (let ((lines (editor-buffer-lines buf)))
      (loop for r from s-row to e-row do
        (let* ((line (copy-seq (aref lines r)))
               (len (length line))
               (c-start (if (= r s-row) (min s-col len) 0))
               (c-end (if (= r e-row) (min (1+ e-col) len) len)))
          (loop for c-idx from c-start below c-end do
            (setf (char line c-idx) (char-downcase (char line c-idx))))
          (setf (aref lines r) line)))))
  (setf (buffer-modified-p buf) t)
  (setf (editor-buffer-status-message buf) "[Lowercase]"))

(defun %switch-to-uppercase (buf)
  "Helix `Alt-`` ` ``: Convert characters in selection to uppercase."
  (%push-undo-snapshot buf)
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (let ((lines (editor-buffer-lines buf)))
      (loop for r from s-row to e-row do
        (let* ((line (copy-seq (aref lines r)))
               (len (length line))
               (c-start (if (= r s-row) (min s-col len) 0))
               (c-end (if (= r e-row) (min (1+ e-col) len) len)))
          (loop for c-idx from c-start below c-end do
            (setf (char line c-idx) (char-upcase (char line c-idx))))
          (setf (aref lines r) line)))))
  (setf (buffer-modified-p buf) t)
  (setf (editor-buffer-status-message buf) "[Uppercase]"))

(defun %indent-selection (buf &optional (spaces 2))
  "Helix `>`: Indent selected lines by SPACES."
  (%push-undo-snapshot buf)
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (declare (ignore s-col e-col))
    (let ((lines (editor-buffer-lines buf))
          (prefix (make-string spaces :initial-element #\Space)))
      (loop for r from s-row to e-row do
        (setf (aref lines r) (concatenate 'string prefix (aref lines r))))))
  (incf (editor-buffer-cursor-col buf) spaces)
  (setf (buffer-modified-p buf) t)
  (setf (editor-buffer-status-message buf) "[Indented]"))

(defun %unindent-selection (buf &optional (spaces 2))
  "Helix `<`: Unindent selected lines by up to SPACES."
  (%push-undo-snapshot buf)
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (declare (ignore s-col e-col))
    (let ((lines (editor-buffer-lines buf)))
      (loop for r from s-row to e-row do
        (let* ((line (aref lines r))
               (trim-count 0))
          (loop while (and (< trim-count spaces)
                           (< trim-count (length line))
                           (char= (char line trim-count) #\Space))
                do (incf trim-count))
          (when (> trim-count 0)
            (setf (aref lines r) (subseq line trim-count)))))))
  (setf (editor-buffer-cursor-col buf) (max 0 (- (editor-buffer-cursor-col buf) spaces)))
  (setf (buffer-modified-p buf) t)
  (setf (editor-buffer-status-message buf) "[Unindented]"))

(defun %join-lines (buf)
  "Helix `J`: Join selected lines with spaces."
  (%push-undo-snapshot buf)
  (multiple-value-bind (s-row s-col e-row e-col) (%get-selection-bounds buf)
    (declare (ignore s-col e-col))
    (let* ((lines (editor-buffer-lines buf))
           (end-r (if (= s-row e-row) (min (1- (length lines)) (1+ s-row)) e-row)))
      (when (< s-row end-r)
        (let ((joined (aref lines s-row)))
          (loop for r from (1+ s-row) to end-r do
            (let ((trimmed (string-left-trim '(#\Space #\Tab) (aref lines r))))
              (setf joined (concatenate 'string joined (if (> (length joined) 0) " " "") trimmed))))
          (setf (aref lines s-row) joined)
          (let ((del-count (- end-r s-row))
                (len (length lines)))
            (loop for i from (1+ s-row) below (- len del-count) do
              (setf (aref lines i) (aref lines (+ i del-count))))
            (decf (fill-pointer lines) del-count))))))
  (setf (editor-buffer-helix-mode buf) :normal
        (editor-buffer-selection-anchor-row buf) nil
        (editor-buffer-selection-anchor-col buf) nil
        (buffer-modified-p buf) t)
  (%clamp-cursor buf))

(defun %jump-matching-bracket (buf)
  "Helix `mm`: Jump to matching bracket or delimiter."
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf))
         (row (editor-buffer-cursor-row buf))
         (lines (editor-buffer-lines buf))
         (ch (if (< col (length cur)) (char cur col) nil)))
    ;; If not directly on bracket, scan forward on current line for bracket
    (unless (and ch (find ch "()[]{}"))
      (let ((pos (position-if (lambda (c) (find c "()[]{}")) cur :start col)))
        (when pos
          (setf (editor-buffer-cursor-col buf) pos
                col pos
                ch (char cur pos))))))
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf))
         (row (editor-buffer-cursor-row buf))
         (lines (editor-buffer-lines buf))
         (ch (if (< col (length cur)) (char cur col) nil)))
    (when (and ch (find ch "()[]{}"))
      (let ((pairs '((#\( . #\)) (#\[ . #\]) (#\{ . #\})
                     (#\) . #\() (#\] . #\[) (#\} . #\{)))
            (forward (find ch "([{")))
        (let ((target (cdr (assoc ch pairs)))
              (depth 1))
          (if forward
              ;; Scan forward
              (loop for r from row below (length lines) do
                (let ((line (aref lines r))
                      (start-c (if (= r row) (1+ col) 0)))
                  (loop for c from start-c below (length line) do
                    (let ((c-char (char line c)))
                      (cond
                        ((char= c-char ch) (incf depth))
                        ((char= c-char target)
                         (decf depth)
                         (when (zerop depth)
                           (setf (editor-buffer-cursor-row buf) r
                                 (editor-buffer-cursor-col buf) c)
                           (return-from %jump-matching-bracket t))))))))
              ;; Scan backward
              (loop for r from row downto 0 do
                (let ((line (aref lines r))
                      (start-c (if (= r row) (1- col) (1- (length (aref lines r))))))
                  (loop for c from start-c downto 0 do
                    (let ((c-char (char line c)))
                      (cond
                        ((char= c-char ch) (incf depth))
                        ((char= c-char target)
                         (decf depth)
                         (when (zerop depth)
                           (setf (editor-buffer-cursor-row buf) r
                                 (editor-buffer-cursor-col buf) c)
                           (return-from %jump-matching-bracket t)))))))))))))
  (%clamp-cursor buf))

(defun %select-word-object (buf &key (around nil))
  "Helix `miw` / `maw`: Select inside or around current word."
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf))
         (len (length cur)))
    (when (and (> len 0) (< col len))
      (let ((start col))
        (loop while (and (> start 0) (%word-char-p (char cur (1- start)))) do (decf start))
        (let ((end col))
          (loop while (and (< end len) (%word-char-p (char cur end))) do (incf end))
          (when around
            (loop while (and (< end len) (char= (char cur end) #\Space)) do (incf end)))
          (when (< start end)
            (setf (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)
                  (editor-buffer-selection-anchor-col buf) start
                  (editor-buffer-cursor-col buf) (max start (1- end))
                  (editor-buffer-status-message buf)
                  (if around "[Selected around word]" "[Selected inside word]"))))))))

(defun %select-delimited-object (buf open-ch close-ch &key (around nil))
  "Helix `mi\"` / `ma\"` / `mi(` / `ma(`: Select inside or around delimited text."
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf))
         (len (length cur)))
    (when (> len 0)
      (let ((start-pos (position open-ch cur :end (min len (1+ col)) :from-end t)))
        (when start-pos
          (let ((end-pos (position close-ch cur :start (1+ start-pos))))
            (when end-pos
              (let ((s (if around start-pos (1+ start-pos)))
                    (e (if around (1+ end-pos) end-pos)))
                (when (<= s e)
                  (setf (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)
                        (editor-buffer-selection-anchor-col buf) s
                        (editor-buffer-cursor-col buf) (max s (1- e))
                        (editor-buffer-status-message buf)
                        (if around "[Selected around]" "[Selected inside]")))))))))))

(defun %insert-at-line-start (buf)
  "Helix `I`: Insert at beginning of line."
  (%push-undo-snapshot buf)
  (let* ((cur (%current-line buf))
         (pos (position-if (lambda (c) (not (find c '(#\Space #\Tab)))) cur)))
    (setf (editor-buffer-cursor-col buf) (or pos 0)
          (editor-buffer-helix-mode buf) :insert
          (editor-buffer-selection-anchor-row buf) nil
          (editor-buffer-selection-anchor-col buf) nil)))

(defun %insert-at-line-end (buf)
  "Helix `A`: Insert at end of line."
  (%push-undo-snapshot buf)
  (setf (editor-buffer-cursor-col buf) (length (%current-line buf))
        (editor-buffer-helix-mode buf) :insert
        (editor-buffer-selection-anchor-row buf) nil
        (editor-buffer-selection-anchor-col buf) nil))

(defun %open-line-below (buf)
  "Helix `o`: Open line below and enter insert mode."
  (%push-undo-snapshot buf)
  (let* ((lines (editor-buffer-lines buf))
         (row (editor-buffer-cursor-row buf))
         (new-len (1+ (length lines))))
    (vector-push-extend "" lines)
    (loop for i from (1- new-len) downto (+ row 2) do
      (setf (aref lines i) (aref lines (1- i))))
    (setf (aref lines (1+ row)) "")
    (incf (editor-buffer-cursor-row buf))
    (setf (editor-buffer-cursor-col buf) 0
          (editor-buffer-helix-mode buf) :insert
          (editor-buffer-selection-anchor-row buf) nil
          (editor-buffer-selection-anchor-col buf) nil
          (buffer-modified-p buf) t)))

(defun %open-line-above (buf)
  "Helix `O`: Open line above and enter insert mode."
  (%push-undo-snapshot buf)
  (let* ((lines (editor-buffer-lines buf))
         (row (editor-buffer-cursor-row buf))
         (new-len (1+ (length lines))))
    (vector-push-extend "" lines)
    (loop for i from (1- new-len) downto (1+ row) do
      (setf (aref lines i) (aref lines (1- i))))
    (setf (aref lines row) "")
    (setf (editor-buffer-cursor-col buf) 0
          (editor-buffer-helix-mode buf) :insert
          (editor-buffer-selection-anchor-row buf) nil
          (editor-buffer-selection-anchor-col buf) nil
          (buffer-modified-p buf) t)))

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

(defun %char-class (c)
  "Classify character into :whitespace, :word (alphanumeric or _), or :punct (symbols/punctuation)."
  (cond
    ((null c) :none)
    ((member c '(#\Space #\Tab #\Newline #\Return)) :whitespace)
    ((or (alphanumericp c) (char= c #\_)) :word)
    (t :punct)))

(defun %word-char-p (c)
  (and c (or (alphanumericp c) (char= c #\_))))

(defun %move-next-word (buf)
  "Helix `w`: Move forward to start of next word or punctuation sequence."
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf))
         (len (length cur)))
    (cond
      ((< col len)
       (let ((i col)
             (init-class (%char-class (char cur col))))
         (if (eq init-class :whitespace)
             (loop while (and (< i len) (eq (%char-class (char cur i)) :whitespace)) do (incf i))
             (progn
               (loop while (and (< i len) (eq (%char-class (char cur i)) init-class)) do (incf i))
               (loop while (and (< i len) (eq (%char-class (char cur i)) :whitespace)) do (incf i))))
         (if (< i len)
             (setf (editor-buffer-cursor-col buf) i)
             (if (< (1+ (editor-buffer-cursor-row buf)) (length (editor-buffer-lines buf)))
                 (progn
                   (incf (editor-buffer-cursor-row buf))
                   (let* ((next-line (%current-line buf))
                          (next-len (length next-line))
                          (ni 0))
                     (loop while (and (< ni next-len) (eq (%char-class (char next-line ni)) :whitespace)) do (incf ni))
                     (setf (editor-buffer-cursor-col buf) (if (< ni next-len) ni 0))))
                 (setf (editor-buffer-cursor-col buf) (max 0 (1- len)))))))
      (t
       (when (< (1+ (editor-buffer-cursor-row buf)) (length (editor-buffer-lines buf)))
         (incf (editor-buffer-cursor-row buf))
         (let* ((next-line (%current-line buf))
                (next-len (length next-line))
                (ni 0))
           (loop while (and (< ni next-len) (eq (%char-class (char next-line ni)) :whitespace)) do (incf ni))
           (setf (editor-buffer-cursor-col buf) (if (< ni next-len) ni 0))))))))

(defun %move-prev-word (buf)
  "Helix `b`: Move backward to start of previous word or punctuation sequence."
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf)))
    (cond
      ((> col 0)
       (let ((i col))
         (loop while (and (> i 0) (eq (%char-class (char cur (1- i))) :whitespace)) do (decf i))
         (when (> i 0)
           (let ((target-class (%char-class (char cur (1- i)))))
             (loop while (and (> i 0) (eq (%char-class (char cur (1- i))) target-class)) do (decf i))))
         (setf (editor-buffer-cursor-col buf) i)))
      (t
       (when (> (editor-buffer-cursor-row buf) 0)
         (decf (editor-buffer-cursor-row buf))
         (let* ((prev-line (%current-line buf))
                (prev-len (length prev-line))
                (i prev-len))
           (loop while (and (> i 0) (eq (%char-class (char prev-line (1- i))) :whitespace)) do (decf i))
           (when (> i 0)
             (let ((target-class (%char-class (char prev-line (1- i)))))
               (loop while (and (> i 0) (eq (%char-class (char prev-line (1- i))) target-class)) do (decf i))))
           (setf (editor-buffer-cursor-col buf) i)))))))

(defun %move-end-word (buf)
  "Helix `e`: Move forward to end of current or next word."
  (let* ((cur (%current-line buf))
         (col (editor-buffer-cursor-col buf))
         (len (length cur)))
    (cond
      ((< col len)
       (let ((i col))
         (loop while (and (< i len) (eq (%char-class (char cur i)) :whitespace)) do (incf i))
         (when (< i len)
           (let ((cls (%char-class (char cur i))))
             (when (and (= i col)
                        (or (>= (1+ i) len)
                            (not (eq (%char-class (char cur (1+ i))) cls))))
               (incf i)
               (loop while (and (< i len) (eq (%char-class (char cur i)) :whitespace)) do (incf i))
               (when (< i len)
                 (setf cls (%char-class (char cur i))))))
           (when (< i len)
             (let ((cls (%char-class (char cur i))))
               (loop while (and (< (1+ i) len) (eq (%char-class (char cur (1+ i))) cls)) do (incf i)))))
         (if (< i len)
             (setf (editor-buffer-cursor-col buf) i)
             (if (< (1+ (editor-buffer-cursor-row buf)) (length (editor-buffer-lines buf)))
                 (progn
                   (incf (editor-buffer-cursor-row buf))
                   (setf (editor-buffer-cursor-col buf) 0)
                   (%move-end-word buf))
                 (setf (editor-buffer-cursor-col buf) (max 0 (1- len)))))))
      (t
       (when (< (1+ (editor-buffer-cursor-row buf)) (length (editor-buffer-lines buf)))
         (incf (editor-buffer-cursor-row buf))
         (setf (editor-buffer-cursor-col buf) 0)
         (%move-end-word buf))))))

(defmacro with-helix-motion ((buf &key head-exclusive backward-word) &body body)
  "Execute a motion in BUF with Helix selection semantics:
   In :normal mode, the motion transiently selects the movement range, replacing prior selection.
   When head-exclusive is true (e.g. w motion), the selection excludes the head character.
   When backward-word is true (e.g. b motion), the selection excludes the character at old position.
   In :select mode, the initial anchor stays fixed and the selection extends."
  (let ((b (gensym "BUF"))
        (old-r (gensym "OLD-ROW"))
        (old-c (gensym "OLD-COL")))
    `(let* ((,b ,buf)
            (,old-r (editor-buffer-cursor-row ,b))
            (,old-c (editor-buffer-cursor-col ,b)))
       (when (and (eq (editor-buffer-helix-mode ,b) :select)
                  (null (editor-buffer-selection-anchor-row ,b)))
         (setf (editor-buffer-selection-anchor-row ,b) ,old-r
               (editor-buffer-selection-anchor-col ,b) ,old-c))
       ,@body
       (%clamp-cursor ,b)
       (let ((new-r (editor-buffer-cursor-row ,b))
             (new-c (editor-buffer-cursor-col ,b)))
         (if (eq (editor-buffer-helix-mode ,b) :normal)
             (cond
               ((and (= new-r ,old-r) (= new-c ,old-c))
                (setf (editor-buffer-selection-anchor-row ,b) nil
                      (editor-buffer-selection-anchor-col ,b) nil
                      (editor-buffer-selection-end-row ,b) nil
                      (editor-buffer-selection-end-col ,b) nil))
               (,head-exclusive
                ;; w motion forward: anchor = old cursor, end = char before new cursor
                (if (= new-r ,old-r)
                    (if (> new-c ,old-c)
                        (setf (editor-buffer-selection-anchor-row ,b) ,old-r
                              (editor-buffer-selection-anchor-col ,b) ,old-c
                              (editor-buffer-selection-end-row ,b) new-r
                              (editor-buffer-selection-end-col ,b) (1- new-c))
                        (setf (editor-buffer-selection-anchor-row ,b) nil
                              (editor-buffer-selection-anchor-col ,b) nil
                              (editor-buffer-selection-end-row ,b) nil
                              (editor-buffer-selection-end-col ,b) nil))
                    ;; Wrapped across lines
                    (if (> new-c 0)
                        (setf (editor-buffer-selection-anchor-row ,b) ,old-r
                              (editor-buffer-selection-anchor-col ,b) ,old-c
                              (editor-buffer-selection-end-row ,b) new-r
                              (editor-buffer-selection-end-col ,b) (1- new-c))
                        (let ((prev-len (length (aref (editor-buffer-lines ,b) (1- new-r)))))
                          (setf (editor-buffer-selection-anchor-row ,b) ,old-r
                                (editor-buffer-selection-anchor-col ,b) ,old-c
                                (editor-buffer-selection-end-row ,b) (1- new-r)
                                (editor-buffer-selection-end-col ,b) (max 0 (1- prev-len)))))))
               (,backward-word
                ;; b motion backward: cursor is at start of target word; selection covers target word up to char before old cursor
                (if (= new-r ,old-r)
                    (if (< new-c ,old-c)
                        (setf (editor-buffer-selection-anchor-row ,b) new-r
                              (editor-buffer-selection-anchor-col ,b) new-c
                              (editor-buffer-selection-end-row ,b) ,old-r
                              (editor-buffer-selection-end-col ,b) (1- ,old-c))
                        (setf (editor-buffer-selection-anchor-row ,b) nil
                              (editor-buffer-selection-anchor-col ,b) nil
                              (editor-buffer-selection-end-row ,b) nil
                              (editor-buffer-selection-end-col ,b) nil))
                    (setf (editor-buffer-selection-anchor-row ,b) new-r
                          (editor-buffer-selection-anchor-col ,b) new-c
                          (editor-buffer-selection-end-row ,b) ,old-r
                          (editor-buffer-selection-end-col ,b) (if (> ,old-c 0) (1- ,old-c) 0))))
               (t
                ;; Standard inclusive motion (e, 0, $, G, etc.)
                (setf (editor-buffer-selection-anchor-row ,b) ,old-r
                      (editor-buffer-selection-anchor-col ,b) ,old-c
                      (editor-buffer-selection-end-row ,b) nil
                      (editor-buffer-selection-end-col ,b) nil)))
             ;; In :select mode: selection extends to current cursor
             (setf (editor-buffer-selection-end-row ,b) nil
                   (editor-buffer-selection-end-col ,b) nil))))))

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
               (setf (lispos.window:window-buffer (symbol-value focus-sym)) sbuf
                     (lispos.window:window-scroll-row (symbol-value focus-sym)) 0
                     (lispos.window:window-scroll-col (symbol-value focus-sym)) 0
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
                              (setf (lispos.window:window-buffer fwin) new-buf
                                    (lispos.window:window-scroll-row fwin) 0
                                    (lispos.window:window-scroll-col fwin) 0
                                    *current-buffer* new-buf))))))))))

      ;; SPC b: Open buffer picker (handles both buffer struct and buffer name string)
      ((#\b :b)
       (setf (editor-buffer-status-message buf) nil)
       (when (fboundp (find-symbol "OPEN-BUFFER-PICKER" :lispos.picker))
         (funcall (find-symbol "OPEN-BUFFER-PICKER" :lispos.picker)
                  (lambda (target)
                    (let ((picked-buf (if (stringp target)
                                          (find-buffer target)
                                          target)))
                      (when picked-buf
                        (when (find-package :lispos.window)
                          (let ((fwin (symbol-value (find-symbol "*FOCUSED-WINDOW*" :lispos.window))))
                            (when fwin
                              (setf (lispos.window:window-buffer fwin) picked-buf
                                    (lispos.window:window-scroll-row fwin) 0
                                    (lispos.window:window-scroll-col fwin) 0
                                    *current-buffer* picked-buf))))))))))

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
         (when (and sbuf (find-package :lispos.window))
           (let ((fwin (symbol-value (find-symbol "*FOCUSED-WINDOW*" :lispos.window))))
             (when fwin
               (setf (lispos.window:window-buffer fwin) sbuf
                     (lispos.window:window-scroll-row fwin) 0
                     (lispos.window:window-scroll-col fwin) 0
                     *current-buffer* sbuf))))))

      ;; SPC y: Yank selection
      ((#\y :y)
       (%yank-selection buf))

      ;; SPC p: Paste clipboard
      ((#\p :p)
       (%paste buf :after t))

      ;; SPC ?: Command palette
      ((#\? :question)
       (when (fboundp (find-symbol "OPEN-COMMAND-PALETTE" :lispos.picker))
         (let ((handler (find-symbol "HANDLE-OS-COMMAND" :lispos.terminal)))
           (when (and handler (fboundp handler))
             (funcall (find-symbol "OPEN-COMMAND-PALETTE" :lispos.picker) (symbol-function handler))))))

      ;; Cancel on Escape / Space
      ((:escape #\Esc #\Space :space)
       (setf (editor-buffer-status-message buf) "[Cancelled]"))

      (t
       (setf (editor-buffer-status-message buf) (format nil "[Unknown leader: ~A]" k))))))

;;; ---------------------------------------------------------------------------
;;; Helix Modal Input Dispatcher
;;; ---------------------------------------------------------------------------

(defmethod buffer-handle-key ((buf editor-buffer) key &key (char nil) (modifiers nil))
  ;; Clear transient status message on new keypress unless leader or pending
  (unless (or (editor-buffer-leader-active-p buf)
              (editor-buffer-pending-command buf))
    (setf (editor-buffer-status-message buf) nil))

  ;; 1. Check if Space leader key is active awaiting secondary key
  (when (editor-buffer-leader-active-p buf)
    (%handle-leader-key buf key char)
    (return-from buffer-handle-key t))

  ;; 2. Check for Alt-; flip selection shortcut
  (when (or (eq key :alt-semicolon)
            (and (member :alt modifiers) (or (eq char #\;) (eq key :semicolon))))
    (%flip-selections buf)
    (return-from buffer-handle-key t))

  ;; 3. Check if a pending multi-key sequence is active
  (when (editor-buffer-pending-command buf)
    (let ((cmd (editor-buffer-pending-command buf))
          (k (if (characterp (or char key)) (or char key) key)))
      (setf (editor-buffer-pending-command buf) nil)
      (case cmd
        ;; Goto sub-mode: g g, g e, g h, g l, g s
        (:go
         (with-helix-motion (buf)
           (cond
             ((eq k #\g)
              (setf (editor-buffer-cursor-row buf) 0
                    (editor-buffer-cursor-col buf) 0))
             ((eq k #\e)
              (setf (editor-buffer-cursor-row buf) (max 0 (1- (length (editor-buffer-lines buf))))
                    (editor-buffer-cursor-col buf) 0))
             ((or (eq k #\h) (eq k #\b))
              (setf (editor-buffer-cursor-col buf) 0))
             ((eq k #\l)
              (setf (editor-buffer-cursor-col buf) (max 0 (1- (length (%current-line buf))))))
             ((eq k #\s)
              (let* ((cur (%current-line buf))
                     (pos (position-if (lambda (c) (not (find c '(#\Space #\Tab)))) cur)))
                (setf (editor-buffer-cursor-col buf) (or pos 0))))))
         (return-from buffer-handle-key t))

        ;; Match sub-mode: m m (jump bracket), m i (inside), m a (around)
        (:match
         (cond
           ((eq k #\m)
            (with-helix-motion (buf)
              (%jump-matching-bracket buf)))
           ((eq k #\i)
            (setf (editor-buffer-pending-command buf) :match-inside
                  (editor-buffer-status-message buf) "[mi] w: word  \": quotes  ': quotes  (: parens")
            (return-from buffer-handle-key t))
           ((eq k #\a)
            (setf (editor-buffer-pending-command buf) :match-around
                  (editor-buffer-status-message buf) "[ma] w: word  \": quotes  ': quotes  (: parens")
            (return-from buffer-handle-key t)))
         (return-from buffer-handle-key t))

        ;; Match Inside text objects (miw, mi", mi', mi()
        (:match-inside
         (cond
           ((eq k #\w) (%select-word-object buf :around nil))
           ((eq k #\") (%select-delimited-object buf #\" #\" :around nil))
           ((eq k #\') (%select-delimited-object buf #\' #\' :around nil))
           ((or (eq k #\() (eq k #\))) (%select-delimited-object buf #\( #\) :around nil))
           ((or (eq k #\[) (eq k #\])) (%select-delimited-object buf #\[ #\] :around nil))
           ((or (eq k #\{) (eq k #\})) (%select-delimited-object buf #\{ #\} :around nil)))
         (return-from buffer-handle-key t))

        ;; Match Around text objects (maw, ma", ma', ma()
        (:match-around
         (cond
           ((eq k #\w) (%select-word-object buf :around t))
           ((eq k #\") (%select-delimited-object buf #\" #\" :around t))
           ((eq k #\') (%select-delimited-object buf #\' #\' :around t))
           ((or (eq k #\() (eq k #\))) (%select-delimited-object buf #\( #\) :around t))
           ((or (eq k #\[) (eq k #\])) (%select-delimited-object buf #\[ #\] :around t))
           ((or (eq k #\{) (eq k #\})) (%select-delimited-object buf #\{ #\} :around t)))
         (return-from buffer-handle-key t))

        (:find-char-fwd
         (when (characterp k)
           (with-helix-motion (buf)
             (%find-char buf k :forward t :till nil)))
         (return-from buffer-handle-key t))

        (:till-char-fwd
         (when (characterp k)
           (with-helix-motion (buf)
             (%find-char buf k :forward t :till t)))
         (return-from buffer-handle-key t))

        (:find-char-bwd
         (when (characterp k)
           (with-helix-motion (buf)
             (%find-char buf k :forward nil :till nil)))
         (return-from buffer-handle-key t))

        (:till-char-bwd
         (when (characterp k)
           (with-helix-motion (buf)
             (%find-char buf k :forward nil :till t)))
         (return-from buffer-handle-key t))

        (:replace-char
         (when (and (characterp k) (graphic-char-p k))
           (%push-undo-snapshot buf)
           (if (or (eq (editor-buffer-helix-mode buf) :select)
                   (and (editor-buffer-selection-anchor-row buf)
                        (editor-buffer-selection-anchor-col buf)))
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

  ;; 4. Route according to active Helix mode
  (case (editor-buffer-helix-mode buf)
    ;; ------------------------------------------------------------------------
    ;; Mode 1: Helix Normal Mode (:normal)
    ;; ------------------------------------------------------------------------
    (:normal
     (let ((k (if (characterp (or char key)) (or char key) key)))
       (cond
         ;; Escape: collapse selection if active
         ((or (eq key :escape) (eq char #\Esc))
          (when (editor-buffer-selection-anchor-row buf)
            (%collapse-selection buf)))

         ;; Space Leader Key
         ((or (eq k #\Space) (eq k :space))
          (setf (editor-buffer-leader-active-p buf) t
                (editor-buffer-status-message buf) "[SPC] f: file  b: buffer  w: save  q: quit  s: scratch"))

         ;; Motions: h, j, k, l / Arrows
         ;; In Normal mode: single-step motions cancel any active selection and move cursor
         ((or (eq k #\h) (eq k :left))
          (setf (editor-buffer-selection-anchor-row buf) nil
                (editor-buffer-selection-anchor-col buf) nil
                (editor-buffer-selection-end-row buf) nil
                (editor-buffer-selection-end-col buf) nil)
          (decf (editor-buffer-cursor-col buf))
          (%clamp-cursor buf))
         ((or (eq k #\l) (eq k :right))
          (setf (editor-buffer-selection-anchor-row buf) nil
                (editor-buffer-selection-anchor-col buf) nil
                (editor-buffer-selection-end-row buf) nil
                (editor-buffer-selection-end-col buf) nil)
          (incf (editor-buffer-cursor-col buf))
          (%clamp-cursor buf))
         ((or (eq k #\j) (eq k :down))
          (setf (editor-buffer-selection-anchor-row buf) nil
                (editor-buffer-selection-anchor-col buf) nil
                (editor-buffer-selection-end-row buf) nil
                (editor-buffer-selection-end-col buf) nil)
          (incf (editor-buffer-cursor-row buf))
          (%clamp-cursor buf))
         ((or (eq k #\k) (eq k :up))
          (setf (editor-buffer-selection-anchor-row buf) nil
                (editor-buffer-selection-anchor-col buf) nil
                (editor-buffer-selection-end-row buf) nil
                (editor-buffer-selection-end-col buf) nil)
          (decf (editor-buffer-cursor-row buf))
          (%clamp-cursor buf))

         ;; Word motions: w, b, e (transient motion selection in normal mode)
         ((eq k #\w)
          (with-helix-motion (buf :head-exclusive t) (%move-next-word buf)))
         ((eq k #\b)
          (with-helix-motion (buf :backward-word t) (%move-prev-word buf)))
         ((eq k #\e)
          (with-helix-motion (buf) (%move-end-word buf)))

         ;; Line Navigation: 0, $
         ((or (eq k #\0) (eq k :home))
          (with-helix-motion (buf) (setf (editor-buffer-cursor-col buf) 0)))
         ((or (eq k #\$) (eq k :end))
          (with-helix-motion (buf) (setf (editor-buffer-cursor-col buf) (max 0 (1- (length (%current-line buf)))))))

         ;; Menus: g (goto), m (match)
         ((eq k #\g)
          (setf (editor-buffer-pending-command buf) :go
                (editor-buffer-status-message buf) "[g] g: top  e: end  h: start  l: end  s: non-blank"))
         ((eq k #\G)
          (with-helix-motion (buf)
            (setf (editor-buffer-cursor-row buf) (max 0 (1- (length (editor-buffer-lines buf))))
                  (editor-buffer-cursor-col buf) 0)))
         ((eq k #\m)
          (setf (editor-buffer-pending-command buf) :match
                (editor-buffer-status-message buf) "[m] m: match bracket  i: inside  a: around"))

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

         ;; Helix Selection Commands: x (line below), X (line bounds), % (all), ; (collapse)
         ((eq k #\x)
          (%select-line-below buf))
         ((eq k #\X)
          (%select-line-bounds buf))
         ((eq k #\%)
          (%select-all buf))
         ((eq k #\;)
          (%collapse-selection buf))

         ;; Enter Visual Select Mode: v
         ((eq k #\v)
          (unless (editor-buffer-selection-anchor-row buf)
            (setf (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)
                  (editor-buffer-selection-anchor-col buf) (editor-buffer-cursor-col buf)))
          (setf (editor-buffer-helix-mode buf) :select
                (editor-buffer-status-message buf) "[Select mode]"))

         ;; Undo / Redo: u, U
         ((eq k #\u) (%undo buf))
         ((eq k #\U) (%redo buf))

         ;; Immediate Selection-First Actions: d (delete), c (change), y (yank), p/P (paste)
         ((or (eq k #\d) (eq k :delete))
          (%delete-selection buf))
         ((eq k #\c)
          (%change-selection buf))
         ((eq k #\y)
          (%yank-selection buf))
         ((eq k #\p)
          (%paste buf :after t))
         ((eq k #\P)
          (%paste buf :after nil))
         ((eq k #\R)
          (%replace-selection-with-yanked buf))
         ((eq k #\r)
          (setf (editor-buffer-pending-command buf) :replace-char
                (editor-buffer-status-message buf) "[r: char to replace]"))

         ;; Case / Format Transformations: ~, `, Alt-`, >, <, J
         ((eq k #\~) (%switch-case buf))
         ((eq k #\`) (%switch-to-lowercase buf))
         ((eq k #\>) (%indent-selection buf))
         ((eq k #\<) (%unindent-selection buf))
         ((eq k #\J) (%join-lines buf))

         ;; Enter Insert Mode: i, a, I, A, o, O
         ((eq k #\i)
          (%push-undo-snapshot buf)
          (setf (editor-buffer-helix-mode buf) :insert))
         ((eq k #\a)
          (%push-undo-snapshot buf)
          (let ((cur-len (length (%current-line buf))))
            (when (< (editor-buffer-cursor-col buf) cur-len)
              (incf (editor-buffer-cursor-col buf))))
          (setf (editor-buffer-helix-mode buf) :insert))
         ((eq k #\I) (%insert-at-line-start buf))
         ((eq k #\A) (%insert-at-line-end buf))
         ((eq k #\o) (%open-line-below buf))
         ((eq k #\O) (%open-line-above buf))

         ;; Command Mode: :
         ((eq k #\:)
          (setf (editor-buffer-helix-mode buf) :command
                (editor-buffer-command-input buf) ""))

         (t nil))))

    ;; ------------------------------------------------------------------------
    ;; Mode 2: Helix Visual Selection Mode (:select)
    ;; ------------------------------------------------------------------------
    (:select
     (let ((k (if (characterp (or char key)) (or char key) key)))
       (cond
         ;; Escape or ';' collapses selection and returns to normal mode
         ((or (eq key :escape) (eq char #\Esc) (eq k #\;))
          (%collapse-selection buf))

         ;; 'v' toggles select mode off, returning to normal mode while keeping selection
         ((eq k #\v)
          (setf (editor-buffer-helix-mode buf) :normal
                (editor-buffer-status-message buf) "[Normal mode]"))

         ;; Space Leader in selection mode
         ((or (eq k #\Space) (eq k :space))
          (setf (editor-buffer-leader-active-p buf) t
                (editor-buffer-status-message buf) "[SPC] f: file  b: buffer  w: save  q: quit  s: scratch"))

         ;; Line selection extend: x (down), X (bounds), % (all)
         ((eq k #\x) (%select-line-below buf))
         ((eq k #\X) (%select-line-bounds buf))
         ((eq k #\%) (%select-all buf))

         ;; Motions extend selection (anchor stays fixed)
         ((or (eq k #\h) (eq k :left))
          (unless (editor-buffer-selection-anchor-row buf)
            (setf (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)
                  (editor-buffer-selection-anchor-col buf) (editor-buffer-cursor-col buf)))
          (decf (editor-buffer-cursor-col buf)) (%clamp-cursor buf))
         ((or (eq k #\l) (eq k :right))
          (unless (editor-buffer-selection-anchor-row buf)
            (setf (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)
                  (editor-buffer-selection-anchor-col buf) (editor-buffer-cursor-col buf)))
          (incf (editor-buffer-cursor-col buf)) (%clamp-cursor buf))
         ((or (eq k #\j) (eq k :down))
          (unless (editor-buffer-selection-anchor-row buf)
            (setf (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)
                  (editor-buffer-selection-anchor-col buf) (editor-buffer-cursor-col buf)))
          (incf (editor-buffer-cursor-row buf)) (%clamp-cursor buf))
         ((or (eq k #\k) (eq k :up))
          (unless (editor-buffer-selection-anchor-row buf)
            (setf (editor-buffer-selection-anchor-row buf) (editor-buffer-cursor-row buf)
                  (editor-buffer-selection-anchor-col buf) (editor-buffer-cursor-col buf)))
          (decf (editor-buffer-cursor-row buf)) (%clamp-cursor buf))
         ((eq k #\w) (with-helix-motion (buf) (%move-next-word buf)))
         ((eq k #\b) (with-helix-motion (buf) (%move-prev-word buf)))
         ((eq k #\e) (with-helix-motion (buf) (%move-end-word buf)))
         ((or (eq k #\0) (eq k :home)) (with-helix-motion (buf) (setf (editor-buffer-cursor-col buf) 0)))
         ((or (eq k #\$) (eq k :end)) (with-helix-motion (buf) (setf (editor-buffer-cursor-col buf) (max 0 (1- (length (%current-line buf)))))))
         ((eq k #\G) (with-helix-motion (buf) (setf (editor-buffer-cursor-row buf) (max 0 (1- (length (editor-buffer-lines buf)))) (editor-buffer-cursor-col buf) 0)))

         ;; Sub-modes in selection mode
         ((eq k #\g)
          (setf (editor-buffer-pending-command buf) :go
                (editor-buffer-status-message buf) "[g] g: top  e: end  h: start  l: end  s: non-blank"))
         ((eq k #\m)
          (setf (editor-buffer-pending-command buf) :match
                (editor-buffer-status-message buf) "[m] m: match bracket  i: inside  a: around"))

         ;; Selection Actions (Immediate): d (delete), c (change), y (yank), r, R, ~, `, >, <, J
         ((or (eq k #\d) (eq k :delete))
          (%delete-selection buf))
         ((eq k #\c)
          (%change-selection buf))
         ((eq k #\y)
          (%yank-selection buf))
         ((eq k #\p)
          (%delete-selection buf)
          (%paste buf :after nil))
         ((eq k #\P)
          (%delete-selection buf)
          (%paste buf :after nil))
         ((eq k #\R)
          (%replace-selection-with-yanked buf))
         ((eq k #\r)
          (setf (editor-buffer-pending-command buf) :replace-char
                (editor-buffer-status-message buf) "[r: char to replace]"))
         ((eq k #\~) (%switch-case buf))
         ((eq k #\`) (%switch-to-lowercase buf))
         ((eq k #\>) (%indent-selection buf))
         ((eq k #\<) (%unindent-selection buf))
         ((eq k #\J) (%join-lines buf))
         ((eq k #\u) (%undo buf))
         ((eq k #\U) (%redo buf))

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

;;; ---------------------------------------------------------------------------
;;; Concrete Subclass: Shell Buffer (Modal Terminal with Helix Scrollback)
;;; ---------------------------------------------------------------------------

(defclass shell-buffer (editor-buffer)
  ((engine
    :initarg :engine
    :initform nil
    :accessor shell-buffer-engine
    :documentation "Underlying shell-engine instance."))
  (:default-initargs :name "*shell*" :helix-mode :insert)
  (:documentation "Buffer hosting an interactive LispOS shell session with Helix modal editing."))

(defmethod initialize-instance :after ((buf shell-buffer) &key &allow-other-keys)
  (unless (shell-buffer-engine buf)
    (setf (shell-buffer-engine buf) (lispos.shell:make-shell-engine)))
  (sync-shell-buffer-lines buf)
  (let* ((eng (shell-buffer-engine buf))
         (hist-len (length (lispos.shell:output-history-lines eng)))
         (prompt-len (length (lispos.shell:engine-prompt eng)))
         (pt (lispos.shell:buffer-point eng)))
    (setf (editor-buffer-cursor-row buf) hist-len
          (editor-buffer-cursor-col buf) (+ prompt-len pt))))

(defmethod buffer-engine ((buf shell-buffer))
  (shell-buffer-engine buf))

(defmethod buffer-mode ((buf shell-buffer))
  :shell)

(defun sync-shell-buffer-lines (buf)
  "Synchronize editor-buffer lines vector from shell-engine history and prompt."
  (let ((eng (shell-buffer-engine buf)))
    (when eng
      (let* ((history (lispos.shell:output-history-lines eng))
             (prompt (lispos.shell:engine-prompt eng))
             (buf-text (lispos.shell:buffer-text eng))
             (current-line (concatenate 'string prompt buf-text))
             (all-lines (append history (list current-line))))
        (setf (editor-buffer-lines buf)
              (make-array (length all-lines)
                          :adjustable t
                          :fill-pointer (length all-lines)
                          :initial-contents all-lines))))))

(defmethod buffer-content-lines ((buf shell-buffer))
  (sync-shell-buffer-lines buf)
  (coerce (editor-buffer-lines buf) 'list))

(defmethod buffer-cursor-position ((buf shell-buffer))
  (sync-shell-buffer-lines buf)
  (if (eq (editor-buffer-helix-mode buf) :insert)
      (let ((eng (shell-buffer-engine buf)))
        (if eng
            (let* ((history-count (length (lispos.shell:output-history-lines eng)))
                   (prompt-len (length (lispos.shell:engine-prompt eng)))
                   (point (lispos.shell:buffer-point eng)))
              (setf (editor-buffer-cursor-row buf) history-count
                    (editor-buffer-cursor-col buf) (+ prompt-len point))
              (values history-count (+ prompt-len point)))
            (values 0 0)))
      (values (editor-buffer-cursor-row buf) (editor-buffer-cursor-col buf))))

(defmethod buffer-handle-key ((buf shell-buffer) key &key (char nil) (modifiers nil))
  (let ((eng (shell-buffer-engine buf)))
    (unless eng
      (return-from buffer-handle-key nil))

    (cond
      ;; ----------------------------------------------------------------------
      ;; In :insert mode: input feeds directly to shell-engine
      ;; ----------------------------------------------------------------------
      ((eq (editor-buffer-helix-mode buf) :insert)
       (cond
         ;; Escape: transition to Normal mode (:normal)
         ((or (eq key :escape) (eq char #\Esc))
          (setf (editor-buffer-helix-mode buf) :normal
                (editor-buffer-selection-anchor-row buf) nil
                (editor-buffer-selection-anchor-col buf) nil
                (editor-buffer-selection-end-row buf) nil
                (editor-buffer-selection-end-col buf) nil)
          (sync-shell-buffer-lines buf)
          (let* ((lines (editor-buffer-lines buf))
                 (last-row (max 0 (1- (length lines))))
                 (last-col (if (> (length lines) 0) (max 0 (1- (length (aref lines last-row)))) 0)))
            (setf (editor-buffer-cursor-row buf) last-row
                  (editor-buffer-cursor-col buf) last-col))
          (setf (editor-buffer-status-message buf) "[Normal mode]")
          t)

         ;; Normal typing / shell keys: forward to shell-engine
         (t
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
            (lispos.shell:handle-key eng effective-key)
            (sync-shell-buffer-lines buf)
            (let* ((hist-len (length (lispos.shell:output-history-lines eng)))
                   (prompt-len (length (lispos.shell:engine-prompt eng)))
                   (pt (lispos.shell:buffer-point eng)))
              (setf (editor-buffer-cursor-row buf) hist-len
                    (editor-buffer-cursor-col buf) (+ prompt-len pt)))
            t))))

      ;; ----------------------------------------------------------------------
      ;; In :normal or :select mode: Helix navigation and clipboard
      ;; ----------------------------------------------------------------------
      (t
       (let ((k (if (characterp (or char key)) (or char key) key))
             (prompt-row (length (lispos.shell:output-history-lines eng))))
         (cond
           ;; Enter insert mode: i, a, I, A
           ((and (eq (editor-buffer-helix-mode buf) :normal)
                 (member k '(#\i #\a #\I #\A)))
            (setf (editor-buffer-helix-mode buf) :insert
                  (editor-buffer-selection-anchor-row buf) nil
                  (editor-buffer-selection-anchor-col buf) nil
                  (editor-buffer-selection-end-row buf) nil
                  (editor-buffer-selection-end-col buf) nil)
            (sync-shell-buffer-lines buf)
            (let ((prompt-len (length (lispos.shell:engine-prompt eng)))
                  (pt (lispos.shell:buffer-point eng)))
              (setf (editor-buffer-cursor-row buf) prompt-row
                    (editor-buffer-cursor-col buf) (+ prompt-len pt)))
            (setf (editor-buffer-status-message buf) "[Insert mode]")
            t)

           ;; Paste into shell prompt: p, P
           ((and (eq (editor-buffer-helix-mode buf) :normal)
                 (or (eq k #\p) (eq k #\P)))
            (let ((clip (editor-buffer-clipboard buf)))
              (if (and clip (> (length clip) 0))
                  (progn
                    (lispos.shell:feed-input eng clip)
                    (sync-shell-buffer-lines buf)
                    (let ((prompt-len (length (lispos.shell:engine-prompt eng)))
                          (pt (lispos.shell:buffer-point eng)))
                      (setf (editor-buffer-cursor-row buf) prompt-row
                            (editor-buffer-cursor-col buf) (+ prompt-len pt)))
                    (setf (editor-buffer-status-message buf) "[Pasted into prompt]"))
                  (setf (editor-buffer-status-message buf) "[Clipboard empty]")))
            t)

           ;; Protect scrollback lines from destructive deletion d / c
           ((and (eq (editor-buffer-helix-mode buf) :normal)
                 (member k '(#\d #\c :delete))
                 (< (editor-buffer-cursor-row buf) prompt-row))
            (setf (editor-buffer-status-message buf) "[Scrollback is read-only; use 'y' to copy]")
            t)

           ;; Otherwise delegate to standard editor-buffer modal handler
           (t
            (call-next-method))))))))

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
