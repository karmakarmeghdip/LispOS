;;;; src/ui/picker.lisp - Centered Floating Fuzzy Picker for LispOS

(in-package :lispos.picker)

(defstruct picker-item
  (title "" :type string)
  (subtitle "" :type string)
  (badge "" :type string)
  (value nil)
  (aliases '() :type list))

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
  "Return items matching current filter string, sorted by match score.
Supports exact and prefix matching against command aliases (e.g. w, wq, q)."
  (let* ((raw-pattern (picker-filter picker))
         (pattern (string-trim '(#\Space #\Tab) raw-pattern))
         (clean-p (if (and (> (length pattern) 0) (char= (char pattern 0) #\:))
                      (subseq pattern 1)
                      pattern))
         (items (picker-items picker)))
    (if (zerop (length pattern))
        items
        (let ((scored '()))
          (dolist (item items)
            (let* ((haystack (format nil "~A ~A ~A"
                                     (picker-item-title item)
                                     (picker-item-subtitle item)
                                     (picker-item-badge item)))
                   (base-score (fuzzy-match-score pattern haystack))
                   (clean-score (and (not (string= clean-p pattern))
                                     (fuzzy-match-score clean-p haystack)))
                   (best-score (max (or base-score 0) (or clean-score 0)))
                   (alias-score 0))
              ;; Check aliases
              (dolist (alias (picker-item-aliases item))
                (let ((clean-a (if (and (> (length alias) 0) (char= (char alias 0) #\:))
                                   (subseq alias 1)
                                   alias)))
                  (cond
                    ;; Exact match on alias (e.g. "w", ":w", "wq")
                    ((or (string-equal pattern alias)
                         (string-equal clean-p clean-a))
                     (setf alias-score (max alias-score 10000)))
                    ;; Prefix match on alias
                    ((and (> (length clean-p) 0)
                          (uiop:string-prefix-p (string-downcase clean-p) (string-downcase clean-a)))
                     (setf alias-score (max alias-score (+ 5000 (* 20 (length clean-p)))))))))
              (let ((final-score (max best-score alias-score)))
                (when (> final-score 0)
                  (push (cons final-score item) scored)))))
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
      ((or (eq key :enter) (eq key :return) (eq char #\Newline) (eq char #\Return))
       (let ((callback (picker-on-select picker))
             (filt (string-trim '(#\Space #\Tab) (picker-filter picker))))
         (close-picker)
         (when callback
           (handler-case
               (cond
                 ;; If user typed a custom command string with arguments (e.g. "w foo.txt" or "e bar.txt")
                 ((and (> (length filt) 0) (find #\Space filt))
                  (funcall callback filt))
                 ;; If filtered matches exist, execute the selected item's value
                 ((and (> count 0) (< (picker-selected-index picker) count))
                  (let ((selected (nth (picker-selected-index picker) filtered)))
                    (funcall callback (picker-item-value selected))))
                 ;; Fallback to raw filter string
                 ((> (length filt) 0)
                  (funcall callback filt)))
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

(defun execute-palette-command (cmd &optional target-buffer)
  "Execute command selected from palette or entered as string alias."
  (let* ((fwin (and (find-package :lispos.window)
                    (symbol-value (find-symbol "*FOCUSED-WINDOW*" :lispos.window))))
         (buf (or target-buffer
                  (and fwin (lispos.window:window-buffer fwin))
                  lispos.buffer:*current-buffer*)))
    (cond
      ;; 1. Buffer / File Operations
      ((or (eq cmd :write) (eq cmd :save))
       (when (and buf (typep buf 'lispos.buffer:editor-buffer))
         (lispos.buffer:save-buffer buf)))

      ((or (eq cmd :write-quit) (eq cmd :save-and-close))
       (when (and buf (typep buf 'lispos.buffer:editor-buffer))
         (lispos.buffer:save-buffer buf)
         (lispos.buffer:kill-buffer buf)))

      ((or (eq cmd :quit) (eq cmd :close-buffer))
       (when buf
         (if (and (lispos.buffer:buffer-modified-p buf)
                  (not (typep buf 'lispos.buffer:scratch-buffer)))
             (when (typep buf 'lispos.buffer:editor-buffer)
               (setf (lispos.buffer:editor-buffer-status-message buf)
                     "[Unsaved changes! Use :q! to force close]"))
             (lispos.buffer:kill-buffer buf))))

      ((or (eq cmd :force-quit) (eq cmd :force-close))
       (when buf
         (lispos.buffer:kill-buffer buf)))

      ;; 2. System / Workspace Actions
      ((eq cmd :scratch)
       (let ((sbuf (lispos.buffer:create-scratch-buffer)))
         (when sbuf
           (when fwin
             (setf (lispos.window:window-buffer fwin) sbuf
                   (lispos.window:window-scroll-row fwin) 0
                   (lispos.window:window-scroll-col fwin) 0))
           (setf lispos.buffer:*current-buffer* sbuf))))

      ((eq cmd :find-file)
       (open-file-picker
        (lambda (path)
          (when path
            (let ((nb (lispos.buffer:open-file-in-buffer path)))
              (when nb
                (when fwin
                  (setf (lispos.window:window-buffer fwin) nb
                        (lispos.window:window-scroll-row fwin) 0
                        (lispos.window:window-scroll-col fwin) 0))
                (setf lispos.buffer:*current-buffer* nb)))))))

      ((eq cmd :switch-buffer)
       (open-buffer-picker
        (lambda (target)
          (let ((picked (if (stringp target) (lispos.buffer:find-buffer target) target)))
            (when picked
              (when fwin
                (setf (lispos.window:window-buffer fwin) picked
                      (lispos.window:window-scroll-row fwin) 0
                      (lispos.window:window-scroll-col fwin) 0))
              (setf lispos.buffer:*current-buffer* picked))))))

      ((eq cmd :vsplit)
       (when (fboundp (find-symbol "SPLIT-WINDOW-VERTICALLY" :lispos.window))
         (funcall (find-symbol "SPLIT-WINDOW-VERTICALLY" :lispos.window))))

      ((eq cmd :hsplit)
       (when (fboundp (find-symbol "SPLIT-WINDOW-HORIZONTALLY" :lispos.window))
         (funcall (find-symbol "SPLIT-WINDOW-HORIZONTALLY" :lispos.window))))

      ((eq cmd :close-win)
       (when (fboundp (find-symbol "CLOSE-WINDOW" :lispos.window))
         (funcall (find-symbol "CLOSE-WINDOW" :lispos.window))))

      ((eq cmd :new-shell)
       (let ((sh (lispos.buffer:create-buffer
                  (format nil "*shell-~D*" (1+ (length (lispos.buffer:list-buffers))))
                  :mode :shell)))
         (when (and sh fwin)
           (setf (lispos.window:window-buffer fwin) sh
                 (lispos.window:window-scroll-row fwin) 0
                 (lispos.window:window-scroll-col fwin) 0)
           (setf lispos.buffer:*current-buffer* sh))))

      ((eq cmd :reboot)
       (format t "[SYS] Rebooting system...~%")
       (lispos.sys:reboot))

      ((eq cmd :poweroff)
       (format t "[SYS] Powering off system...~%")
       (lispos.sys:poweroff))

      ;; 3. String command parsing (e.g. "w", ":w", "wq", "w path", "e path", "q", "q!")
      ((stringp cmd)
       (let* ((trimmed (string-trim '(#\Space #\Tab) cmd))
              (raw (if (and (> (length trimmed) 0) (char= (char trimmed 0) #\:))
                       (subseq trimmed 1)
                       trimmed))
              (space-pos (position #\Space raw))
              (verb (if space-pos (subseq raw 0 space-pos) raw))
              (arg (if space-pos (string-trim '(#\Space #\Tab) (subseq raw (1+ space-pos))) nil)))
         (cond
           ((or (string= verb "w") (string= verb "write"))
            (when (and buf (typep buf 'lispos.buffer:editor-buffer))
              (if (and arg (> (length arg) 0))
                  (lispos.buffer:save-buffer buf arg)
                  (lispos.buffer:save-buffer buf))))

           ((or (string= verb "wq") (string= verb "x"))
            (when (and buf (typep buf 'lispos.buffer:editor-buffer))
              (if (and arg (> (length arg) 0))
                  (lispos.buffer:save-buffer buf arg)
                  (lispos.buffer:save-buffer buf))
              (lispos.buffer:kill-buffer buf)))

           ((or (string= verb "q") (string= verb "quit"))
            (execute-palette-command :quit buf))

           ((or (string= verb "q!") (string= verb "quit!"))
            (execute-palette-command :force-quit buf))

           ((or (string= verb "e") (string= verb "edit"))
            (if (and arg (> (length arg) 0))
                (let ((nb (lispos.buffer:open-file-in-buffer arg)))
                  (when nb
                    (when fwin
                      (setf (lispos.window:window-buffer fwin) nb
                            (lispos.window:window-scroll-row fwin) 0
                            (lispos.window:window-scroll-col fwin) 0))
                    (setf lispos.buffer:*current-buffer* nb)))
                (when (typep buf 'lispos.buffer:editor-buffer)
                  (setf (lispos.buffer:editor-buffer-status-message buf) "Usage: :e <filename>"))))

           ((string= verb "scratch")
            (execute-palette-command :scratch buf))

           (t
            (when (typep buf 'lispos.buffer:editor-buffer)
              (setf (lispos.buffer:editor-buffer-status-message buf)
                    (format nil "Unknown command: ~A" cmd))))))))))

(defun open-command-palette (&optional on-execute-command)
  "Open command palette picker (used by Super+P and ':' in normal mode)."
  (let* ((active-buf (or (and (find-package :lispos.window)
                              (symbol-value (find-symbol "*FOCUSED-WINDOW*" :lispos.window))
                              (lispos.window:window-buffer (symbol-value (find-symbol "*FOCUSED-WINDOW*" :lispos.window))))
                         lispos.buffer:*current-buffer*))
         (handler (or on-execute-command
                      (lambda (cmd) (execute-palette-command cmd active-buf))))
         (commands
           (list
            (make-picker-item :title ":w / Save Buffer"
                              :subtitle "Write current buffer to disk (:w / SPC w)"
                              :badge "File"
                              :value :write
                              :aliases '("w" ":w" "write" ":write" "save"))
            (make-picker-item :title ":wq / Save and Close"
                              :subtitle "Write buffer to disk and close (:wq)"
                              :badge "File"
                              :value :write-quit
                              :aliases '("wq" ":wq" "x" ":x" "save and quit" "save & quit"))
            (make-picker-item :title ":q / Close Buffer"
                              :subtitle "Close current buffer (:q / SPC q)"
                              :badge "File"
                              :value :quit
                              :aliases '("q" ":q" "quit" ":quit" "close"))
            (make-picker-item :title ":q! / Force Close Buffer"
                              :subtitle "Force close current buffer discarding unsaved changes (:q!)"
                              :badge "File"
                              :value :force-quit
                              :aliases '("q!" ":q!" "quit!" ":quit!"))
            (make-picker-item :title "Open Scratch Buffer"
                              :subtitle "Switch to interactive *scratch* editor (SPC s)"
                              :badge "Buf"
                              :value :scratch
                              :aliases '("scratch" ":scratch" "sc"))
            (make-picker-item :title "Find File"
                              :subtitle "Open file in editor (SPC f / Super+f)"
                              :badge "File"
                              :value :find-file
                              :aliases '("find" "file" "e" ":e" "edit" ":edit" "open"))
            (make-picker-item :title "Switch Buffer"
                              :subtitle "Switch active buffer (SPC b / Super+b)"
                              :badge "Buf"
                              :value :switch-buffer
                              :aliases '("buffer" "b" ":b" "buf"))
            (make-picker-item :title "Split Window Vertically"
                              :subtitle "Split current viewport side-by-side (Super+v)"
                              :badge "Win"
                              :value :vsplit
                              :aliases '("vsplit" ":vsplit" "vsp" ":vsp"))
            (make-picker-item :title "Split Window Horizontally"
                              :subtitle "Split current viewport top-and-bottom (Super+s)"
                              :badge "Win"
                              :value :hsplit
                              :aliases '("split" ":split" "sp" ":sp" "hsplit"))
            (make-picker-item :title "Close Focused Window"
                              :subtitle "Close current viewport (Super+w)"
                              :badge "Win"
                              :value :close-win
                              :aliases '("close" "window" "close-win"))
            (make-picker-item :title "New Shell Buffer"
                              :subtitle "Create and switch to new *shell* REPL buffer"
                              :badge "Buf"
                              :value :new-shell
                              :aliases '("shell" "term" "terminal"))
            (make-picker-item :title "Reboot LispOS"
                              :subtitle "Safely synchronize filesystems and reboot"
                              :badge "Sys"
                              :value :reboot
                              :aliases '("reboot" "restart"))
            (make-picker-item :title "Power Off"
                              :subtitle "Safely halt and power down system"
                              :badge "Sys"
                              :value :poweroff
                              :aliases '("poweroff" "halt" "shutdown")))))
    (open-picker "Commands" commands handler :width 680)))
