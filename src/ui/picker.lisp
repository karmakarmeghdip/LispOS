;;;; src/ui/picker.lisp - Centered Floating Fuzzy Picker for LispOS

(in-package :lispos.picker)

(defstruct picker-item
  (title "" :type string)
  (subtitle "" :type string)
  (badge "" :type string)
  (value nil))

(defstruct picker
  (title "Command Palette" :type string)
  (items '() :type list)
  (filter "" :type string)
  (selected-index 0 :type fixnum)
  (on-select nil)                     ; callback (lambda (item-value))
  (visible-p t :type boolean)
  (width 680 :type fixnum)
  (item-height 28 :type fixnum)
  (max-visible 8 :type fixnum))

(defvar *active-picker* nil
  "Currently active modal picker popup, or nil if none is open.")

(defun fuzzy-match-score (pattern text)
  "Return integer match score if PATTERN characters appear in order in TEXT, or nil."
  (let ((p-len (length pattern))
        (t-len (length text))
        (score 0)
        (last-match -1)
        (p-idx 0))
    (declare (type fixnum p-len t-len score last-match p-idx))
    (when (zerop p-len)
      (return-from fuzzy-match-score 1))
    (loop for t-idx of-type fixnum from 0 below t-len do
      (when (< p-idx p-len)
        (let ((pc (char-downcase (char pattern p-idx)))
              (tc (char-downcase (char text t-idx))))
          (when (char= pc tc)
            (incf score 10)
            ;; Consecutive character bonus
            (when (= t-idx (1+ last-match))
              (incf score 15))
            ;; Start of word / camel-case bonus
            (when (or (zerop t-idx)
                      (member (char text (1- t-idx)) '(#\Space #\- #\_ #\/ #\.)))
              (incf score 20))
            (setf last-match t-idx)
            (incf p-idx)))))
    (if (= p-idx p-len)
        score
        nil)))

(defun picker-filtered-items (picker)
  "Return items matching current filter string, sorted by match score."
  (let ((pattern (picker-filter picker))
        (items (picker-items picker)))
    (if (zerop (length pattern))
        items
        (let ((scored '()))
          (dolist (item items)
            (let* ((haystack (format nil "~A ~A ~A" (picker-item-title item) (picker-item-subtitle item) (picker-item-badge item)))
                   (score (fuzzy-match-score pattern haystack)))
              (when score
                (push (cons score item) scored))))
          (mapcar #'cdr (sort scored #'> :key #'car))))))

(defun open-picker (title items on-select &key (width 680))
  "Open a modal popup picker."
  (setf *active-picker*
        (make-picker :title title
                     :items items
                     :filter ""
                     :selected-index 0
                     :on-select on-select
                     :visible-p t
                     :width width)))

(defun close-picker ()
  "Dismiss active modal picker."
  (setf *active-picker* nil))

(defun picker-handle-key (picker key &key (char nil) (modifiers nil))
  "Handle key navigation inside modal picker."
  (declare (ignore modifiers))
  (let* ((filtered (picker-filtered-items picker))
         (count (length filtered)))
    (cond
      ;; Escape / C-g / C-c closes picker
      ((or (eq key :escape) (eq key :ctrl-g) (eq key :ctrl-c) (eq char #\Esc))
       (close-picker)
       t)

      ;; Up / Down navigation
      ((or (eq key :up) (eq key :ctrl-p))
       (when (> count 0)
         (setf (picker-selected-index picker)
               (mod (1- (picker-selected-index picker)) count)))
       t)

      ((or (eq key :down) (eq key :ctrl-n))
       (when (> count 0)
         (setf (picker-selected-index picker)
               (mod (1+ (picker-selected-index picker)) count)))
       t)

      ;; Enter confirms selection
      ((or (eq key :enter) (eq char #\Newline) (eq char #\Return))
       (let ((callback (picker-on-select picker)))
         (close-picker)
         (when callback
           (handler-case
               (if (and (> count 0) (< (picker-selected-index picker) count))
                   (let ((selected (nth (picker-selected-index picker) filtered)))
                     (funcall callback (picker-item-value selected)))
                   ;; If user typed a custom filename or input that didn't match
                   (when (> (length (picker-filter picker)) 0)
                     (funcall callback (picker-filter picker))))
             (error (c)
               (format t "[PICKER Error] Selection callback error: ~A~%" c)))))
       t)

      ;; Backspace removes filter character
      ((or (eq key :backspace) (eq char #\Backspace) (eq char #\Rubout))
       (let ((flen (length (picker-filter picker))))
         (when (> flen 0)
           (setf (picker-filter picker) (subseq (picker-filter picker) 0 (1- flen))
                 (picker-selected-index picker) 0)))
       t)

      ;; Typing characters updates filter
      ((characterp (or char key))
       (let ((c (or char key)))
         (when (graphic-char-p c)
           (setf (picker-filter picker) (concatenate 'string (picker-filter picker) (string c))
                 (picker-selected-index picker) 0)))
       t)

      (t nil))))

;; Standard Picker Workflows

(defun open-buffer-picker (on-select-buffer)
  "Open floating buffer switcher picker."
  (let* ((buffers (lispos.buffer:list-buffers))
         (items (loop for buf in buffers
                      for i from 1
                      collect (make-picker-item
                                :title (lispos.buffer:buffer-name buf)
                                :subtitle (format nil "mode: ~A~A"
                                                  (string-downcase (symbol-name (lispos.buffer:buffer-mode buf)))
                                                  (if (lispos.buffer:buffer-file-path buf)
                                                      (format nil "  [~A]" (lispos.buffer:buffer-file-path buf))
                                                      ""))
                                :badge (format nil "#~D" i)
                                :value buf))))
    (open-picker "Buffers" items on-select-buffer :width 640)))

(defun open-file-picker (on-select-file)
  "Open floating file finder picker."
  (let* ((candidates (append (ignore-errors (directory "/Users/Admin/*.*"))
                             (ignore-errors (directory "/Users/Admin/**/*.*"))
                             (ignore-errors (directory "/System/Source/LispOS/*.*"))
                             (ignore-errors (directory "/System/Source/LispOS/src/**/*.*"))
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
                   (make-picker-item :title "Open Scratch Buffer" :subtitle "Switch to interactive *scratch* editor (SPC s)" :value :scratch :badge "Buf")
                   (make-picker-item :title "Find File" :subtitle "Open file in editor (SPC f / Super+f)" :value :find-file :badge "File")
                   (make-picker-item :title "Switch Buffer" :subtitle "Switch active buffer (SPC b / Super+b)" :value :switch-buffer :badge "Buf")
                   (make-picker-item :title "Split Window Vertically" :subtitle "Split current viewport side-by-side (Super+v)" :value :vsplit :badge "Win")
                   (make-picker-item :title "Split Window Horizontally" :subtitle "Split current viewport top-and-bottom (Super+s)" :value :hsplit :badge "Win")
                   (make-picker-item :title "Close Focused Window" :subtitle "Close current viewport (Super+w)" :value :close-win :badge "Win")
                   (make-picker-item :title "New Shell Buffer" :subtitle "Create and switch to new *shell* REPL buffer" :value :new-shell :badge "Buf")
                   (make-picker-item :title "Reboot LispOS" :subtitle "Safely synchronize filesystems and reboot" :value :reboot :badge "Sys")
                   (make-picker-item :title "Power Off" :subtitle "Safely halt and power down system" :value :poweroff :badge "Sys"))))
    (open-picker "Commands" commands on-execute-command :width 680)))
