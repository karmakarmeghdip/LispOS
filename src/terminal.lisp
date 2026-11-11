;;;; src/terminal.lisp - Live Interactive Graphical Workspace for LispOS

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
      ;; 2. Initialize Tiling Window Tree Root (occupying full screen above modeline)
      (let ((root (lispos.window:make-window
                   :buffer shell-buf
                   :x 0
                   :y 0
                   :width w
                   :height workspace-h)))
        (setf lispos.window:*root-window* root
              lispos.window:*focused-window* root)))

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
  "Render single unified modeline at the bottom of the screen matching Emacs/Doom desktop style."
  (let* ((disp (terminal-display term))
         (fnt (terminal-font term))
         (w (lispos.display:display-width disp))
         (h (lispos.display:display-height disp))
         (mh (terminal-modeline-height term))
         (my (- h mh))
         (ty (+ my 4))
         ;; Left title: current path / buffer
         (left-title (lispos.window:active-window-modeline-info))
         ;; Center clock: HH:MM:SS
         (clock-text (multiple-value-bind (s m hr) (decode-universal-time (get-universal-time))
                       (format nil "~2,'0D:~2,'0D:~2,'0D" hr m s)))
         (clock-w (lispos.font:measure-text fnt clock-text))
         (clock-x (round (- w clock-w) 2))
         ;; Right system status indicators: Bluetooth, Wi-Fi, Battery
         (bt-icon (string (code-char #xF293)))
         (wifi-icon (string (code-char #xF1EB)))
         (bat-icon (string (code-char #xF0079)))
         (bat-pct " 100%")
         (spacer "  ")
         (total-right-w (+ (lispos.font:measure-text fnt bt-icon)
                           (lispos.font:measure-text fnt spacer)
                           (lispos.font:measure-text fnt wifi-icon)
                           (lispos.font:measure-text fnt spacer)
                           (lispos.font:measure-text fnt bat-icon)
                           (lispos.font:measure-text fnt bat-pct)))
         (rx (- w total-right-w 16)))
    ;; Background and top border line
    (lispos.display:fill-rect disp 0 my w mh +modeline-bg+)
    (lispos.display:fill-rect disp 0 my w 1 +win-border+)

    ;; 1. Left: Active buffer / path title
    (lispos.font:draw-text disp fnt 16 ty left-title +modeline-fg+)

    ;; 2. Center: Clock HH:MM:SS
    (lispos.font:draw-text disp fnt clock-x ty clock-text #xFFBAC2DE)

    ;; 3. Right: Bluetooth, Wi-Fi, Battery
    (setf rx (lispos.font:draw-text disp fnt rx ty bt-icon #xFFCDD6F4))
    (setf rx (lispos.font:draw-text disp fnt rx ty spacer #xFFCDD6F4))
    (setf rx (lispos.font:draw-text disp fnt rx ty wifi-icon #xFFCDD6F4))
    (setf rx (lispos.font:draw-text disp fnt rx ty spacer #xFFCDD6F4))
    (setf rx (lispos.font:draw-text disp fnt rx ty bat-icon #xFFA6E3A1))
    (lispos.font:draw-text disp fnt rx ty bat-pct #xFFA6E3A1)))

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
      (let* ((lines (lispos.buffer:buffer-content-lines buf))
             (num-lines (length lines))
             (start-idx (max 0 (- num-lines max-lines)))
             (visible-lines (subseq lines start-idx num-lines))
             (cur-y (+ wy margin-y)))

        ;; Draw buffer lines
        (dolist (line visible-lines)
          (render-output-line disp fnt (+ wx margin-x) cur-y line (- ww (* 2 margin-x)))
          (incf cur-y lh))

        ;; Draw Cursor if Focused
        (when focused-p
          (case (lispos.buffer:buffer-mode buf)
            (:shell
             (let ((eng (lispos.buffer:buffer-engine buf)))
               (when eng
                 (let* ((prompt (lispos.shell:engine-prompt eng))
                        (point (lispos.shell:buffer-point eng))
                        (buf-text (lispos.shell:buffer-text eng))
                        (prompt-w (lispos.font:measure-text fnt prompt))
                        (cursor-x (+ wx margin-x prompt-w (* point cw)))
                        (cursor-y (- cur-y lh))
                        (cursor-char (if (< point (length buf-text))
                                         (char buf-text point)
                                         #\Space)))
                   (lispos.display:fill-rect disp cursor-x cursor-y cw lh +cursor-color+)
                   (lispos.font:draw-char-at disp fnt cursor-x cursor-y cursor-char +cursor-fg+)))))))))))

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
                  for (item . nil) in filtered do
                    (let* ((selected-p (= i (lispos.picker:picker-selected-index picker)))
                           (bg (if selected-p +picker-sel-bg+ nil))
                           (fg (if selected-p +picker-sel-fg+ #xFFCDD6F4))
                           (badge (lispos.picker::picker-item-badge item)))
                      (when bg
                        (lispos.display:fill-rect disp (+ px 8) item-y (- pw 16) (- item-h 2) bg))
                      ;; Pointer indicator
                      (lispos.font:draw-text disp fnt (+ px 16) (+ item-y 4)
                                             (if selected-p ">" " ")
                                             #xFFF9E2AF)
                      ;; Title
                      (lispos.font:draw-text disp fnt (+ px 36) (+ item-y 4)
                                             (lispos.picker::picker-item-title item)
                                             fg)
                      ;; Subtitle
                      (when (lispos.picker::picker-item-subtitle item)
                        (lispos.font:draw-text disp fnt (+ px 240) (+ item-y 4)
                                               (lispos.picker::picker-item-subtitle item)
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
                    (let ((buf (lispos.buffer:create-buffer
                                (file-namestring file-path)
                                :mode :file
                                :file-path file-path)))
                      (when lispos.window:*focused-window*
                        (setf (lispos.window:window-buffer lispos.window:*focused-window*) buf))))))
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
  (handler-case
      (loop while (terminal-running-p term) do
        (let ((handled-any nil))
          ;; Drain all currently queued input events
          (loop for ev = (lispos.input:poll-input-event)
                while ev do
                  (when (eq (lispos.input:input-event-type ev) :key-down)
                    (terminal-handle-key term
                                         (lispos.input:input-event-key ev)
                                         :char (lispos.input:input-event-char ev)
                                         :modifiers (lispos.input:input-event-modifiers ev))
                    (setf handled-any t))))
        ;; Sleep ~16ms (~60Hz tick)
        (sleep 0.016))
    (error (c)
      (format t "[TERMINAL] Loop caught condition: ~A~%" c))))

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
