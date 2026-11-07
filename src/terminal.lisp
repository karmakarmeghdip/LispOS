;;;; src/terminal.lisp - Live Interactive Graphical Terminal for LispOS

(in-package :lispos.terminal)

(defparameter +bg-color+      #xFF181825) ; Crust / Dark base
(defparameter +header-bg+      #xFF1E1E2E) ; Mantle
(defparameter +header-fg+      #xFF89B4FA) ; Blue
(defparameter +prompt-lambda+  #xFFA6E3A1) ; Green
(defparameter +prompt-arrow+   #xFF89B4FA) ; Blue
(defparameter +input-text+     #xFFCDD6F4) ; Text
(defparameter +cursor-color+   #xFFF5E0DC) ; Rosewater
(defparameter +cursor-fg+      #xFF11111B) ; Black text under cursor
(defparameter +eval-result+    #xFFF9E2AF) ; Yellow
(defparameter +stdout-text+    #xFFBAC2DE) ; Subtext1
(defparameter +stderr-text+    #xFFF38BA8) ; Red
(defparameter +sep-line+       #xFF313244) ; Surface0

(defstruct terminal
  (engine nil)
  (display nil)
  (font nil)
  (cols 80 :type fixnum)
  (rows 25 :type fixnum)
  (header-height 32 :type fixnum)
  (margin-x 20 :type fixnum)
  (margin-y 40 :type fixnum)
  (running-p nil :type boolean)
  (thread nil)
  (dirty-p t :type boolean))

(defvar *terminal* nil
  "Active primary interactive graphical terminal instance.")

(defun init-terminal (&key (display lispos.display:*display*)
                           (font lispos.font:*font*)
                           (engine nil))
  "Initialize graphical terminal shell."
  (let* ((disp (or display (lispos.display:init-display)))
         (fnt (or font (lispos.font:init-font)))
         (eng (or engine (lispos.shell:make-shell-engine)))
         (w (lispos.display:display-width disp))
         (h (lispos.display:display-height disp))
         (cw (lispos.font:font-char-width fnt))
         (lh (lispos.font:font-line-height fnt))
         (margin-x 20)
         (margin-y 40)
         (usable-w (- w (* 2 margin-x)))
         (usable-h (- h margin-y 20))
         (cols (max 40 (floor usable-w cw)))
         (rows (max 10 (floor usable-h lh))))
    (setf *terminal*
          (make-terminal :engine eng
                         :display disp
                         :font fnt
                         :cols cols
                         :rows rows
                         :margin-x margin-x
                         :margin-y margin-y
                         :dirty-p t))
    (render-terminal *terminal*)
    (lispos.display:present-display disp)
    *terminal*))

(defun render-header (term)
  "Render top status bar."
  (let* ((disp (terminal-display term))
         (fnt (terminal-font term))
         (w (lispos.display:display-width disp))
         (h (terminal-header-height term)))
    (lispos.display:fill-rect disp 0 0 w h +header-bg+)
    (lispos.display:fill-rect disp 0 h w 2 +sep-line+)
    (lispos.font:draw-text disp fnt 20 6
                           "LispOS  •  Interactive Monolithic Lisp Shell"
                           +header-fg+)
    (let ((hint "Cascadia Code NF  |  (eval)  |  C-l: clear"))
      (let* ((hint-w (lispos.font:measure-text fnt hint))
             (hx (max 400 (- w hint-w 20))))
        (lispos.font:draw-text disp fnt hx 6 hint #xFF6C7086)))))

(defun render-output-line (disp fnt x y line-text)
  "Draw a single line of output with semantic syntax coloring."
  (cond
    ;; Evaluation return value line: "=> ..."
    ((and (>= (length line-text) 2) (string= (subseq line-text 0 2) "=>"))
     (lispos.font:draw-text disp fnt x y line-text +eval-result+))
    ;; Error line: "[Error] ..." or "[ABORT]"
    ((or (and (>= (length line-text) 7) (string= (subseq line-text 0 7) "[Error]"))
         (search "[ABORT]" line-text))
     (lispos.font:draw-text disp fnt x y line-text +stderr-text+))
    ;; Prompt input echo line: "λ > ..."
    ((and (>= (length line-text) 4) (search "λ > " line-text))
     (let ((lambda-x (lispos.font:draw-text disp fnt x y "λ" +prompt-lambda+)))
       (let ((arrow-x (lispos.font:draw-text disp fnt lambda-x y " > " +prompt-arrow+)))
         (when (> (length line-text) 4)
           (lispos.font:draw-text disp fnt arrow-x y (subseq line-text 4) +input-text+)))))
    ;; Standard stdout line
    (t
     (lispos.font:draw-text disp fnt x y line-text +stdout-text+))))

(defun render-terminal (term)
  "Full screen redraw of terminal onto the double-buffered display backbuffer."
  (let* ((disp (terminal-display term))
         (fnt (terminal-font term))
         (eng (terminal-engine term))
         (w (lispos.display:display-width disp))
         (h (lispos.display:display-height disp))
         (cw (lispos.font:font-char-width fnt))
         (lh (lispos.font:font-line-height fnt))
         (mx (terminal-margin-x term))
         (my (terminal-margin-y term))
         (max-visible-rows (- (terminal-rows term) 2)))

    ;; 1. Clear background
    (lispos.display:fill-rect disp 0 0 w h +bg-color+)

    ;; 2. Render Header
    (render-header term)

    ;; 3. Gather output history lines
    (let* ((history (lispos.shell:output-history-lines eng))
           (num-history (length history))
           ;; Scroll so latest lines are visible right above current prompt
           (start-idx (max 0 (- num-history max-visible-rows)))
           (visible-history (subseq history start-idx num-history))
           (cur-y my))

      ;; Draw historical lines
      (dolist (line visible-history)
        (render-output-line disp fnt mx cur-y line)
        (incf cur-y lh))

      ;; 4. Draw active prompt and input buffer line
      (let* ((prompt (lispos.shell:engine-prompt eng))
             (buffer (lispos.shell:buffer-text eng))
             (point (lispos.shell:buffer-point eng)))

        ;; Draw 'λ' in green, ' > ' in blue
        (let ((px mx))
          (if (search "λ" prompt)
              (progn
                (setf px (lispos.font:draw-text disp fnt px cur-y "λ" +prompt-lambda+))
                (setf px (lispos.font:draw-text disp fnt px cur-y " > " +prompt-arrow+)))
              (setf px (lispos.font:draw-text disp fnt px cur-y prompt +prompt-arrow+)))

          ;; Draw input buffer characters
          (let ((buf-start-x px))
            (loop for i from 0 below (length buffer) do
              (let ((ch (char buffer i))
                    (cx (+ buf-start-x (* i cw))))
                (lispos.font:draw-char-at disp fnt cx cur-y ch +input-text+)))

            ;; Draw cursor block at point
            (let* ((cursor-x (+ buf-start-x (* point cw)))
                   (cursor-char (if (< point (length buffer)) (char buffer point) #\Space)))
              (lispos.display:fill-rect disp cursor-x cur-y cw lh +cursor-color+)
              (lispos.font:draw-char-at disp fnt cursor-x cur-y cursor-char +cursor-fg+)))))))
  (setf (terminal-dirty-p term) nil))

(defun terminal-handle-key (term key &key (char nil) (modifiers nil))
  "Dispatch key event to shell engine and redraw if state changed."
  (let ((eng (terminal-engine term)))
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
        (lispos.shell:handle-key eng effective-key)
        (render-terminal term)
        (lispos.display:present-display (terminal-display term))))))

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
