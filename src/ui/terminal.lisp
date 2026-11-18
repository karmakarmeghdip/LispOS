;;;; src/ui/terminal.lisp - Live Interactive Graphical Workspace for LispOS

(in-package :lispos.terminal)

;; Catppuccin Mocha Color Palette
(defparameter +bg-color+      #xFF181825) ; Crust / Dark base
(defparameter +win-bg+        #xFF1E1E2E) ; Base window background
(defparameter +win-border+    #xFF313244) ; Split window divider line
(defparameter +focus-border+  #xFF89B4FA) ; Focused window accent border
(defparameter +modeline-bg+   #xFF181825) ; Unified bottom modeline background
(defparameter +modeline-fg+   #xFFCDD6F4) ; Unified bottom modeline text
(defparameter +prompt-lambda+  #xFFA6E3A1) ; Green
(defparameter +prompt-arrow+   #xFF89B4FA) ; Blue
(defparameter +input-text+     #xFFCDD6F4) ; Text
(defparameter +cursor-color+   #xFFF5E0DC) ; Rosewater
(defparameter +cursor-fg+      #xFF11111B) ; Black text under cursor
(defparameter +eval-result+    #xFFF9E2AF) ; Yellow
(defparameter +stdout-text+    #xFFBAC2DE) ; Subtext1
(defparameter +stderr-text+    #xFFF38BA8) ; Red
(defparameter +picker-bg+      #xFF181825) ; Modal popup background
(defparameter +picker-border+  #xFF89B4FA) ; Modal popup blue border
(defparameter +picker-sel-bg+  #xFF313244) ; Modal selected item highlight
(defparameter +picker-sel-fg+  #xFFF9E2AF) ; Modal selected item text

(defstruct terminal
  (engine nil)
  (display nil)
  (font nil)
  (cols 80 :type fixnum)
  (rows 25 :type fixnum)
  (modeline-height 28 :type fixnum)
  (running-p nil :type boolean)
  (thread nil)
  (dirty-p t :type boolean))

(defvar *terminal* nil
  "Active primary interactive graphical terminal instance.")

(defun init-terminal (&key (display lispos.display:*display*)
                           (font lispos.font:*font*)
                           (engine nil))
  "Initialize graphical workspace, tiling window tree, and default buffers."
  (let* ((disp (or display (lispos.display:init-display)))
         (fnt (or font (lispos.font:init-font)))
         (w (lispos.display:display-width disp))
         (h (lispos.display:display-height disp))
         (cw (lispos.font:font-char-width fnt))
         (lh (lispos.font:font-line-height fnt))
         (modeline-h 28)
         (workspace-h (- h modeline-h))
         (cols (max 40 (floor (- w 40) cw)))
         (rows (max 10 (floor workspace-h lh))))

    ;; 1. Initialize Default Primary Shell Buffer
    (let ((shell-buf (lispos.buffer:create-buffer "*shell*"
                                                  :mode :shell
                                                  :engine (or engine (lispos.shell:make-shell-engine)))))
      ;; Also pre-create *scratch* buffer in registry
      (lispos.buffer:create-scratch-buffer)
      ;; 2. Initialize Tiling Window Tree Root (occupying full screen above modeline)
      (let ((root (lispos.window:make-window
                   :buffer shell-buf
                   :x 0
                   :y 0
                   :width w
                   :height workspace-h)))
        (setf lispos.window:*root-window* root
              lispos.window:*focused-window* root
              lispos.buffer:*current-buffer* shell-buf)))

    (setf *terminal*
          (make-terminal :engine (lispos.buffer:buffer-engine (lispos.window:window-buffer lispos.window:*focused-window*))
                         :display disp
                         :font fnt
                         :cols cols
                         :rows rows
                         :modeline-height modeline-h
                         :dirty-p t))
    (render-terminal *terminal*)
    (lispos.display:present-display disp)
    *terminal*))

(defun render-unified-modeline (term)
  "Render minimal unified modeline at the bottom: <mode-icon>  <file-or-buffer-name>."
  (let* ((disp (terminal-display term))
         (fnt (terminal-font term))
         (w (lispos.display:display-width disp))
         (h (lispos.display:display-height disp))
         (mh (terminal-modeline-height term))
         (my (- h mh))
         (ty (+ my 4))
         (win lispos.window:*focused-window*)
         (buf (and win (lispos.window:window-buffer win))))
    ;; Background and top border line
    (lispos.display:fill-rect disp 0 my w mh +modeline-bg+)
    (lispos.display:fill-rect disp 0 my w 1 +win-border+)

    (when buf
      (let* ((icon-char (lispos.buffer:buffer-mode-icon buf))
             (mode-color (if (typep buf 'lispos.buffer:editor-buffer)
                             (case (lispos.buffer:editor-buffer-helix-mode buf)
                               (:insert #xFFA6E3A1) ; green for insert
                               (:select #xFFF9E2AF) ; yellow for select
                               (:command #xFFF38BA8); peach for command
                               (t #xFF89B4FA))      ; blue for normal
                             #xFF89B4FA))
             (name (lispos.buffer:buffer-name buf))
             (mod-str (if (lispos.buffer:buffer-modified-p buf) " *" ""))
             (title (concatenate 'string name mod-str))
             (cur-x 16))
        ;; 1. Mode icon in semantic color
        (setf cur-x (lispos.font:draw-text disp fnt cur-x ty (string icon-char) mode-color))
        ;; 2. Separator space
        (setf cur-x (lispos.font:draw-text disp fnt cur-x ty "  " +modeline-fg+))
        ;; 3. File / buffer name
        (lispos.font:draw-text disp fnt cur-x ty title +modeline-fg+)))))

(defun render-output-line (disp fnt x y line-text max-w)
  "Draw a single line of output with semantic syntax coloring."
  (declare (ignore max-w))
  (cond
    ;; Return value line: "=> ..."
    ((and (>= (length line-text) 2) (string= (subseq line-text 0 2) "=>"))
     (lispos.font:draw-text disp fnt x y line-text +eval-result+))
    ;; Error line: "[Error] ..." or "[ABORT]"
    ((or (and (>= (length line-text) 7) (string= (subseq line-text 0 7) "[Error]"))
         (search "[ABORT]" line-text))
     (lispos.font:draw-text disp fnt x y line-text +stderr-text+))
    ;; Shell prompt line: "λ [...] > ..." or "λ > ..."
    ((and (>= (length line-text) 2) (string= (subseq line-text 0 2) "λ "))
     (let* ((arrow-pos (search " > " line-text)))
       (if arrow-pos
           (let* ((prefix (subseq line-text 0 arrow-pos))
                  (cmd (if (< (+ arrow-pos 3) (length line-text))
                           (subseq line-text (+ arrow-pos 3))
                           ""))
                  (px (lispos.font:draw-text disp fnt x y prefix +prompt-lambda+))
                  (ax (lispos.font:draw-text disp fnt px y " > " +prompt-arrow+)))
             (lispos.font:draw-text disp fnt ax y cmd +input-text+))
           (lispos.font:draw-text disp fnt x y line-text +prompt-lambda+))))
    ;; Standard stdout line
    (t
     (lispos.font:draw-text disp fnt x y line-text +stdout-text+))))

(defun render-window (disp fnt win focused-p)
  "Render individual tiling window leaf onto backbuffer."
  (let* ((wx (lispos.window:window-x win))
         (wy (lispos.window:window-y win))
         (ww (lispos.window:window-width win))
         (wh (lispos.window:window-height win))
         (buf (lispos.window:window-buffer win))
         (cw (lispos.font:font-char-width fnt))
         (lh (lispos.font:font-line-height fnt))
         (margin-x 12)
         (margin-y 8)
         (max-lines (max 1 (floor (- wh (* 2 margin-y)) lh))))

    ;; 1. Draw Window Background & Border
    (lispos.display:fill-rect disp wx wy ww wh +win-bg+)
    (let ((border-col (if focused-p +focus-border+ +win-border+)))
      ;; Draw 1px border around window
      (lispos.display:fill-rect disp wx wy ww 1 border-col)
      (lispos.display:fill-rect disp wx (+ wy wh -1) ww 1 border-col)
      (lispos.display:fill-rect disp wx wy 1 wh border-col)
      (lispos.display:fill-rect disp (+ wx ww -1) wy 1 wh border-col))

    ;; 2. Render Buffer Content
    (when buf
      (multiple-value-bind (cursor-row cursor-col) (lispos.buffer:buffer-cursor-position buf)
        (let* ((lines (lispos.buffer:buffer-content-lines buf))
               (num-lines (length lines))
               (has-gutter (and (typep buf 'lispos.buffer:editor-buffer)
                                (not (typep buf 'lispos.buffer:shell-buffer))))
               (digits (if has-gutter (max 2 (length (format nil "~D" num-lines))) 0))
               (gutter-w (if has-gutter (+ (* (+ digits 2) cw) 8) 0))
               (text-x (+ wx margin-x gutter-w))
               (start-idx
                 (progn
                   (when (< cursor-row (lispos.window:window-scroll-row win))
                     (setf (lispos.window:window-scroll-row win) cursor-row))
                   (when (>= cursor-row (+ (lispos.window:window-scroll-row win) max-lines))
                     (setf (lispos.window:window-scroll-row win) (1+ (- cursor-row max-lines))))
                   (max 0 (min (lispos.window:window-scroll-row win) (max 0 (1- num-lines))))))
               (end-idx (min num-lines (+ start-idx max-lines)))
               (cur-y (+ wy margin-y)))

          ;; Draw lines with optional gutter and selection highlighting
          (loop for line-idx from start-idx below end-idx do
            (let ((line (nth line-idx lines)))
              ;; Line number gutter for editor buffers
              (when has-gutter
                (let ((is-cmd-line (and (eq (lispos.buffer:editor-buffer-helix-mode buf) :command)
                                        (= line-idx (length (lispos.buffer:editor-buffer-lines buf))))))
                  (if is-cmd-line
                      ;; Blank gutter on command input line
                      (lispos.font:draw-text disp fnt (+ wx margin-x) cur-y
                                             (make-string (+ digits 2) :initial-element #\Space)
                                             #xFF585B70)
                      (let* ((is-cur (= line-idx cursor-row))
                             (ln-col (if (and is-cur focused-p) #xFFCDD6F4 #xFF585B70))
                             (ln-str (format nil "~VD │" digits (1+ line-idx))))
                        (lispos.font:draw-text disp fnt (+ wx margin-x) cur-y ln-str ln-col)))))

              ;; Visual selection highlight in :select mode or when selection active
              (when (and (typep buf 'lispos.buffer:editor-buffer)
                         (or (eq (lispos.buffer:editor-buffer-helix-mode buf) :select)
                             (lispos.buffer:editor-buffer-selection-anchor-row buf)))
                (multiple-value-bind (s-row s-col e-row e-col) (lispos.buffer::%get-selection-bounds buf)
                  (when (and (>= line-idx s-row) (<= line-idx e-row))
                    (let* ((line-len (length line))
                           (c-start (if (= line-idx s-row) s-col 0))
                           (c-end (if (= line-idx e-row) (min (1+ e-col) (max 1 line-len)) (max 1 line-len)))
                           (span (max 1 (- c-end c-start)))
                           (hl-x (+ text-x (* c-start cw)))
                           (hl-w (* span cw)))
                      (lispos.display:fill-rect disp hl-x cur-y hl-w lh #xFF45475A)))))

              ;; Draw line text
              (render-output-line disp fnt text-x cur-y line (- ww margin-x gutter-w))
              (incf cur-y lh)))

          ;; Draw Cursor if Focused and in visible range
          (when (and focused-p (>= cursor-row start-idx) (< cursor-row end-idx))
            (let* ((vis-line-idx (- cursor-row start-idx))
                   (cursor-y (+ wy margin-y (* vis-line-idx lh)))
                   (cursor-x (+ text-x (* cursor-col cw)))
                   (line (if (< cursor-row num-lines) (nth cursor-row lines) ""))
                   (cursor-char (if (< cursor-col (length line))
                                    (char line cursor-col)
                                    #\Space))
                   (is-insert (and (typep buf 'lispos.buffer:editor-buffer)
                                   (eq (lispos.buffer:editor-buffer-helix-mode buf) :insert))))
              (if is-insert
                  ;; Insert mode: 2px vertical bar cursor
                  (lispos.display:fill-rect disp cursor-x cursor-y 2 lh +cursor-color+)
                  ;; Normal / Command / Select / Shell mode: full block cursor
                  (progn
                    (lispos.display:fill-rect disp cursor-x cursor-y cw lh +cursor-color+)
                    (lispos.font:draw-char-at disp fnt cursor-x cursor-y cursor-char +cursor-fg+))))))))))

(defun render-picker-overlay (disp fnt picker)
  "Render centered floating modal fuzzy picker."
  (when (and picker (lispos.picker:picker-visible-p picker))
    (let* ((dw (lispos.display:display-width disp))
           (dh (lispos.display:display-height disp))
           (pw (lispos.picker:picker-width picker))
           (filtered (lispos.picker:picker-filtered-items picker))
           (item-count (min (length filtered) (lispos.picker:picker-max-visible picker)))
           (item-h (lispos.picker:picker-item-height picker))
           (ph (+ 60 (* (max 1 item-count) item-h) 16))
           (px (round (- dw pw) 2))
           (py (round (- dh ph) 3)) ; Golden ratio upper-center placement
           (cw (lispos.font:font-char-width fnt)))

      ;; Modal shadow / border
      (lispos.display:fill-rect disp (- px 2) (- py 2) (+ pw 4) (+ ph 4) +picker-border+)
      (lispos.display:fill-rect disp px py pw ph +picker-bg+)

      ;; Title & Query Filter bar
      (lispos.display:fill-rect disp px py pw 36 #xFF1E1E2E)
      (lispos.font:draw-text disp fnt (+ px 16) (+ py 8)
                             (format nil "~A ~A" (string (code-char #xF002)) (lispos.picker:picker-title picker))
                             #xFF89B4FA)

      ;; Search Input Box
      (let* ((filter-str (lispos.picker:picker-filter picker))
             (prompt-str (if (zerop (length filter-str)) "Type to filter..." filter-str))
             (prompt-fg (if (zerop (length filter-str)) #xFF6C7086 #xFFCDD6F4)))
        (lispos.font:draw-text disp fnt (+ px 180) (+ py 8) prompt-str prompt-fg)
        ;; Cursor in search field
        (unless (zerop (length filter-str))
          (let ((cx (+ px 180 (* (length filter-str) cw))))
            (lispos.display:fill-rect disp cx (+ py 8) 2 18 #xFFF9E2AF))))

      ;; Divider line
      (lispos.display:fill-rect disp px (+ py 36) pw 1 +win-border+)

      ;; List Items
      (let ((item-y (+ py 44)))
        (if (null filtered)
            (lispos.font:draw-text disp fnt (+ px 20) (+ item-y 6) "No matching entries" #xFF6C7086)
            (loop for i from 0 below item-count
                  for item in filtered do
                    (let* ((selected-p (= i (lispos.picker:picker-selected-index picker)))
                           (bg (if selected-p +picker-sel-bg+ nil))
                           (fg (if selected-p +picker-sel-fg+ #xFFCDD6F4))
                           (badge (lispos.picker:picker-item-badge item)))
                      (when bg
                        (lispos.display:fill-rect disp (+ px 8) item-y (- pw 16) (- item-h 2) bg))
                      ;; Pointer indicator
                      (lispos.font:draw-text disp fnt (+ px 16) (+ item-y 4)
                                             (if selected-p ">" " ")
                                             #xFFF9E2AF)
                      ;; Title
                      (lispos.font:draw-text disp fnt (+ px 36) (+ item-y 4)
                                             (lispos.picker:picker-item-title item)
                                             fg)
                      ;; Subtitle
                      (when (lispos.picker:picker-item-subtitle item)
                        (lispos.font:draw-text disp fnt (+ px 240) (+ item-y 4)
                                               (lispos.picker:picker-item-subtitle item)
                                               #xFF6C7086))
                      ;; Right-side badge
                      (when badge
                        (let ((bx (- (+ px pw) (lispos.font:measure-text fnt badge) 24)))
                          (lispos.font:draw-text disp fnt bx (+ item-y 4) badge #xFF89B4FA)))
                      (incf item-y item-h))))))))

(defun render-terminal (term)
  "Render full desktop: all tiled windows, bottom unified modeline, and popup picker."
  (let* ((disp (terminal-display term))
         (fnt (terminal-font term))
         (w (lispos.display:display-width disp))
         (h (lispos.display:display-height disp)))

    ;; 1. Clear background
    (lispos.display:fill-rect disp 0 0 w h +bg-color+)

    ;; 2. Render All Leaf Windows (full height from top to modeline)
    (let ((leaves (lispos.window:all-leaf-windows))
          (focused lispos.window:*focused-window*))
      (dolist (win leaves)
        (render-window disp fnt win (eq win focused))))

    ;; 3. Render Unified Bottom Modeline
    (render-unified-modeline term)

    ;; 4. Render Centered Floating Picker Overlay (if active)
    (when lispos.picker:*active-picker*
      (render-picker-overlay disp fnt lispos.picker:*active-picker*)))
  (setf (terminal-dirty-p term) nil))

(defun handle-os-command (cmd)
  "Execute an OS command from picker / shortcuts."
  (case cmd
    (:scratch
     (let ((buf (lispos.buffer:create-scratch-buffer)))
       (when lispos.window:*focused-window*
         (setf (lispos.window:window-buffer lispos.window:*focused-window*) buf)
         (setf lispos.buffer:*current-buffer* buf))))
    (:find-file
     (lispos.picker:open-file-picker
      (lambda (path)
        (when path
          (let ((buf (lispos.buffer:open-file-in-buffer path)))
            (when lispos.window:*focused-window*
              (setf (lispos.window:window-buffer lispos.window:*focused-window*) buf)
              (setf lispos.buffer:*current-buffer* buf)))))))
    (:switch-buffer
     (lispos.picker:open-buffer-picker
      (lambda (buf)
        (when (and buf lispos.window:*focused-window*)
          (setf (lispos.window:window-buffer lispos.window:*focused-window*) buf)
          (setf lispos.buffer:*current-buffer* buf)))))
    (:vsplit (lispos.window:split-window-vertically))
    (:hsplit (lispos.window:split-window-horizontally))
    (:close-win (lispos.window:close-window))
    (:new-shell
     (let ((buf (lispos.buffer:create-buffer (format nil "*shell-~D*" (1+ (length (lispos.buffer:list-buffers)))) :mode :shell)))
       (when lispos.window:*focused-window*
         (setf (lispos.window:window-buffer lispos.window:*focused-window*) buf))))
    (:reboot
     (format t "[SYS] Rebooting system...~%")
     (lispos.sys:reboot))
    (:poweroff
     (format t "[SYS] Powering off system...~%")
     (lispos.sys:poweroff))))

(defun terminal-handle-key (term key &key (char nil) (modifiers nil))
  "Global event router: handles Super shortcuts, active modal pickers, and buffer edits."
  ;; 1. Check if a floating modal picker is active
  (if lispos.picker:*active-picker*
      (progn
        (lispos.picker:picker-handle-key lispos.picker:*active-picker* key :char char :modifiers modifiers)
        (render-terminal term)
        (lispos.display:present-display (terminal-display term)))

      ;; 2. Check for Global Super-key OS Shortcuts
      (if (member :super modifiers)
          (let ((k (if (characterp (or char key)) (char-downcase (or char key)) key)))
            (case k
              ;; Super+b: Centered Buffer Switcher Picker
              ((#\b :b)
               (lispos.picker:open-buffer-picker
                (lambda (buf)
                  (when (and buf lispos.window:*focused-window*)
                    (setf (lispos.window:window-buffer lispos.window:*focused-window*) buf)
                    (setf lispos.buffer:*current-buffer* buf))))
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))

              ;; Super+f: Centered File Finder Picker
              ((#\f :f)
               (lispos.picker:open-file-picker
                (lambda (file-path)
                  (when file-path
                    (let ((buf (lispos.buffer:open-file-in-buffer file-path)))
                      (when lispos.window:*focused-window*
                        (setf (lispos.window:window-buffer lispos.window:*focused-window*) buf)
                        (setf lispos.buffer:*current-buffer* buf))))))
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))

              ;; Super+p: Command Palette
              ((#\p :p)
               (lispos.picker:open-command-palette #'handle-os-command)
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))

              ;; Window Split: Super+v (vertical side-by-side), Super+s (horizontal top/bottom)
              ((#\v :v)
               (lispos.window:split-window-vertically)
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))

              ((#\s :s)
               (lispos.window:split-window-horizontally)
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))

              ;; Window Close: Super+w
              ((#\w :w)
               (lispos.window:close-window)
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))

              ;; Window Directional Focus: Super+h/j/k/l or Super+Arrows
              ((#\h :left)
               (lispos.window:focus-direction :left)
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))

              ((#\l :right)
               (lispos.window:focus-direction :right)
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))

              ((#\k :up)
               (lispos.window:focus-direction :up)
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))

              ((#\j :down)
               (lispos.window:focus-direction :down)
               (render-terminal term)
               (lispos.display:present-display (terminal-display term)))))

          ;; 3. Route to Focused Window's Buffer
          (let* ((win lispos.window:*focused-window*)
                 (buf (and win (lispos.window:window-buffer win))))
            (when buf
              (lispos.buffer:buffer-handle-key buf key :char char :modifiers modifiers)
              (render-terminal term)
              (lispos.display:present-display (terminal-display term)))))))

(defun terminal-event-loop (term)
  "Background event dispatcher polling evdev input and updating screen."
  (format t "[TERMINAL] Interactive GUI Shell event loop started.~%")
  (loop while (terminal-running-p term) do
    (handler-case
        (let ((handled-any nil))
          ;; Drain all currently queued input events
          (loop for ev = (lispos.input:poll-input-event)
                while ev do
                  (when (eq (lispos.input:input-event-type ev) :key-down)
                    (terminal-handle-key term
                                         (lispos.input:input-event-key ev)
                                         :char (lispos.input:input-event-char ev)
                                         :modifiers (lispos.input:input-event-modifiers ev))
                    (setf handled-any t)))
          (sleep 0.016))
      (error (c)
        (format t "[TERMINAL Error] Event loop caught condition: ~A~%" c)
        (sleep 0.05)))))

(defun start-terminal-thread (&optional (term *terminal*))
  "Start the terminal event dispatcher in a dedicated thread."
  (let ((t-inst (or term *terminal*)))
    (when (and t-inst (not (terminal-running-p t-inst)))
      (setf (terminal-running-p t-inst) t)
      (setf (terminal-thread t-inst)
            (sb-thread:make-thread (lambda () (terminal-event-loop t-inst))
                                   :name "LispOS-Terminal-GUI"))
      t-inst)))

(defun stop-terminal-thread (&optional (term *terminal*))
  "Stop the terminal event dispatcher."
  (let ((t-inst (or term *terminal*)))
    (when (and t-inst (terminal-running-p t-inst))
      (setf (terminal-running-p t-inst) nil)
      (when (terminal-thread t-inst)
        (ignore-errors (sb-thread:terminate-thread (terminal-thread t-inst)))
        (setf (terminal-thread t-inst) nil))
      t-inst)))
