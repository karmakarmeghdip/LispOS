;;;; src/picker.lisp - Centered Floating Fuzzy Picker for LispOS

(in-package :lispos.picker)

(defstruct picker-item
  (title "" :type string)
  (subtitle "" :type string)
  (value nil)
  (badge nil :type (or null string)))

(defstruct picker
  (title "Picker" :type string)
  (items '() :type list)
  (filter "" :type string)
  (selected-index 0 :type fixnum)
  (on-select nil)
  (visible-p t :type boolean)
  (width 680 :type fixnum)
  (item-height 28 :type fixnum)
  (max-visible 8 :type fixnum))

(defvar *active-picker* nil
  "Currently active popup picker, or nil if none open.")

(defun fuzzy-match-score (pattern text)
  "Calculate fuzzy substring matching score. Returns integer score or nil if no match.
   Higher score indicates better match (consecutive letters, prefix matches receive bonuses)."
  (let ((pat (string-downcase pattern))
        (str (string-downcase text)))
    (if (zerop (length pat))
        100
        (let ((p-idx 0)
              (p-len (length pat))
              (s-len (length str))
              (score 0)
              (consecutive 0)
              (prev-match-idx -2))
          (loop for s-idx from 0 below s-len do
            (when (and (< p-idx p-len) (char= (char pat p-idx) (char str s-idx)))
              (incf score 10)
              (if (= s-idx (1+ prev-match-idx))
                  (progn
                    (incf consecutive)
                    (incf score (* consecutive 5)))
                  (setf consecutive 0))
              (when (zerop s-idx)
                (incf score 20)) ; Prefix match bonus
              (setf prev-match-idx s-idx)
              (incf p-idx)))
          (if (= p-idx p-len)
              score
              nil)))))

(defun picker-filtered-items (picker)
  "Return list of (item . score) matching picker's current filter, sorted by highest score."
  (let ((query (picker-filter picker))
        (all-items (picker-items picker)))
    (if (zerop (length query))
        (mapcar (lambda (it) (cons it 0)) all-items)
        (let ((matches '()))
          (dolist (item all-items)
            (let ((score (or (fuzzy-match-score query (picker-item-title item))
                             (fuzzy-match-score query (picker-item-subtitle item)))))
              (when score
                (push (cons item score) matches))))
          (sort matches #'> :key #'cdr)))))

(defun open-picker (title items on-select &key (width 680))
  "Open a centered floating modal picker."
  (setf *active-picker*
        (make-picker :title title
                     :items items
                     :filter ""
                     :selected-index 0
                     :on-select on-select
                     :visible-p t
                     :width width))
  *active-picker*)

(defun close-picker ()
  "Dismiss current picker."
  (setf *active-picker* nil))

(defun picker-handle-key (picker key &key (char nil) (modifiers nil))
  "Process key input inside the modal picker. Returns T if handled."
  (declare (ignore modifiers))
  (let* ((filtered (picker-filtered-items picker))
         (count (length filtered)))
    (cond
      ;; Escape: dismiss picker
      ((or (eq key :escape) (eq key :ctrl-c) (eq key :ctrl-g))
       (close-picker)
       t)

      ;; Enter: activate selected item
      ((or (eq key :enter) (eq key #\Newline))
       (when (and (> count 0) (< (picker-selected-index picker) count))
         (let* ((selected-item (car (nth (picker-selected-index picker) filtered)))
                (handler (picker-on-select picker)))
           (close-picker)
           (when handler
             (funcall handler (picker-item-value selected-item)))))
       t)

      ;; Up / C-p / C-k: navigate up
      ((or (eq key :up) (eq key :ctrl-p) (eq key :ctrl-k))
       (when (> count 0)
         (setf (picker-selected-index picker)
               (mod (1- (picker-selected-index picker)) count)))
       t)

      ;; Down / C-n / C-j: navigate down
      ((or (eq key :down) (eq key :ctrl-n) (eq key :ctrl-j))
       (when (> count 0)
         (setf (picker-selected-index picker)
               (mod (1+ (picker-selected-index picker)) count)))
       t)

      ;; Backspace: delete character from search filter
      ((or (eq key :backspace) (eq char #\Backspace))
       (let ((f (picker-filter picker)))
         (when (> (length f) 0)
           (setf (picker-filter picker) (subseq f 0 (1- (length f))))
           (setf (picker-selected-index picker) 0)))
       t)

      ;; Printable character: append to search filter
      ((characterp (or char key))
       (let ((ch (or char key)))
         (unless (member (char-code ch) '(10 13 27 127 8))
           (setf (picker-filter picker)
                 (concatenate 'string (picker-filter picker) (string ch)))
           (setf (picker-selected-index picker) 0)
           t)))

      (t nil))))

;;; ---------------------------------------------------------------------------
;;; Specialized Pickers
;;; ---------------------------------------------------------------------------

(defun open-buffer-picker (on-select-buffer)
  "Open floating buffer switcher picker."
  (let* ((buffers (lispos.buffer:list-buffers))
         (items (loop for buf in buffers
                      for i from 1
                      collect (make-picker-item
                               :title (lispos.buffer:buffer-name buf)
                               :subtitle (format nil "Mode: ~A~A"
                                                 (string-downcase (symbol-name (lispos.buffer:buffer-mode buf)))
                                                 (if (lispos.buffer:buffer-file-path buf)
                                                     (format nil "  [~A]" (lispos.buffer:buffer-file-path buf))
                                                     ""))
                               :badge (format nil "#~D" i)
                               :value buf))))
    (open-picker "Buffers" items on-select-buffer :width 640)))

(defun open-file-picker (on-select-file)
  "Open floating file finder picker."
  (let* ((candidates (append (ignore-errors (directory "/System/Source/LispOS/*.*"))
                             (ignore-errors (directory "/System/Source/LispOS/src/*.*"))
                             (ignore-errors (directory "/System/Configuration/*.*"))
                             (ignore-errors (directory "/*.*"))))
         (items (loop for file in candidates
                      when (and file (pathname-name file))
                      collect (make-picker-item
                               :title (file-namestring file)
                               :subtitle (namestring file)
                               :badge (or (pathname-type file) "file")
                               :value (namestring file)))))
    (open-picker "Find File" items on-select-file :width 720)))

(defun open-command-palette (on-execute-command)
  "Open command palette picker."
  (let ((commands (list
                   (make-picker-item :title "Split Window Vertically" :subtitle "Split current viewport side-by-side (Super+v)" :value :vsplit :badge "Win")
                   (make-picker-item :title "Split Window Horizontally" :subtitle "Split current viewport top-and-bottom (Super+s)" :value :hsplit :badge "Win")
                   (make-picker-item :title "Close Focused Window" :subtitle "Close current viewport (Super+w)" :value :close-win :badge "Win")
                   (make-picker-item :title "New Shell Buffer" :subtitle "Create and switch to new *shell* REPL buffer" :value :new-shell :badge "Buf")
                   (make-picker-item :title "New Scratch Buffer" :subtitle "Create a clean Common Lisp scratchpad buffer" :value :new-scratch :badge "Buf")
                   (make-picker-item :title "Reboot LispOS" :subtitle "Safely synchronize filesystems and reboot" :value :reboot :badge "Sys")
                   (make-picker-item :title "Power Off" :subtitle "Halt processor and power off machine" :value :poweroff :badge "Sys"))))
    (open-picker "Commands" commands on-execute-command :width 680)))
