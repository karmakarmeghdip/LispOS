;;;; src/shell/engine.lisp - Dual-Mode Shell & Lisp Evaluation Dispatcher

(in-package :lispos.shell)

(defun evaluate-lisp-string (engine code-str)
  "Evaluate Common Lisp expression(s) in CODE-STR.
Captures *standard-output*, *error-output*, and multiple return values.
Updates engine state and returns :submitted or :error."
  (let ((in (make-string-input-stream code-str))
        (stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream))
        (results nil)
        (has-results nil)
        (eval-error nil)
        (*package* (shell-engine-package engine)))
    (handler-case
        (let ((*standard-output* stdout-stream)
              (*error-output* stderr-stream))
          (loop
            (let ((form (read in nil :eof)))
              (when (eq form :eof)
                (return))
              (setf results (multiple-value-list (eval form)))
              (setf has-results t))))
      (error (c)
        (setf eval-error c)))

    (let ((stdout-text (get-output-stream-string stdout-stream))
          (stderr-text (get-output-stream-string stderr-stream)))
      ;; Update engine metadata
      (setf (shell-engine-last-stdout engine) stdout-text)
      (setf (shell-engine-last-stderr engine) stderr-text)
      (setf (shell-engine-last-error engine) eval-error)

      ;; Record captured stdout lines in output history
      (when (> (length stdout-text) 0)
        (dolist (line (split-lines stdout-text))
          (push line (shell-engine-output-history engine))))

      ;; Record captured stderr lines in output history
      (when (> (length stderr-text) 0)
        (dolist (line (split-lines stderr-text))
          (push (format nil "[STDERR] ~A" line) (shell-engine-output-history engine))))

      (if eval-error
          (let ((err-msg (format nil "[ERROR] ~A" eval-error)))
            (setf (shell-engine-last-eval-values engine) nil)
            (setf (shell-engine-last-status engine) :error)
            (push err-msg (shell-engine-output-history engine))
            :error)
          (progn
            (setf (shell-engine-last-eval-values engine) results)
            (setf (shell-engine-last-status engine) :submitted)
            (when has-results
              (dolist (val results)
                (push (format nil "=> ~S" val) (shell-engine-output-history engine))))
            :submitted)))))

(defun evaluate-shell-line (engine line-str)
  "Execute command line LINE-STR using shell dispatch (implicit top-level parenthesis)."
  (let* ((tokens (tokenize-shell-line line-str))
         (stdout-stream (make-string-output-stream))
         (stderr-stream (make-string-output-stream))
         (results nil)
         (eval-error nil)
         (*package* (shell-engine-package engine)))
    (when (null tokens)
      (setf (shell-engine-last-eval-values engine) nil)
      (setf (shell-engine-last-stdout engine) "")
      (setf (shell-engine-last-stderr engine) "")
      (setf (shell-engine-last-error engine) nil)
      (setf (shell-engine-last-status engine) :empty)
      (return-from evaluate-shell-line :empty))

    (let* ((cmd-name (first tokens))
           (args (rest tokens))
           (handler (get-shell-command cmd-name)))
      (handler-case
          (let ((*standard-output* stdout-stream)
                (*error-output* stderr-stream))
            (cond
              ;; 1. Registered shell builtin
              (handler
               (let ((ret (apply handler engine args)))
                 (when ret
                   (setf results (if (listp ret) ret (list ret))))))

              ;; 2. Direct Common Lisp function lookup in current package or CL-USER
              ((let ((sym (find-symbol (string-upcase cmd-name) *package*)))
                 (and sym (fboundp sym)))
               (let* ((sym (find-symbol (string-upcase cmd-name) *package*))
                      (fn (fdefinition sym))
                      (parsed-args (mapcar (lambda (a)
                                             (or (ignore-errors (parse-integer a))
                                                 a))
                                           args))
                      (ret (multiple-value-list (apply fn parsed-args))))
                 (setf results ret)))

              ;; 3. Command not found
              (t
               (format stderr-stream
                       "[SHELL] Command not found: ~A~%Type 'help' for available commands or use '(~A ...)' for Common Lisp evaluation.~%"
                       cmd-name cmd-name))))
        (error (c)
          (setf eval-error c)))

      (let ((stdout-text (get-output-stream-string stdout-stream))
            (stderr-text (get-output-stream-string stderr-stream)))
        (setf (shell-engine-last-stdout engine) stdout-text)
        (setf (shell-engine-last-stderr engine) stderr-text)
        (setf (shell-engine-last-error engine) eval-error)

        (when (> (length stdout-text) 0)
          (dolist (line (split-lines stdout-text))
            (push line (shell-engine-output-history engine))))

        (when (> (length stderr-text) 0)
          (dolist (line (split-lines stderr-text))
            (push (if (search "[SHELL]" line)
                      line
                      (format nil "[STDERR] ~A" line))
                  (shell-engine-output-history engine))))

        (if eval-error
            (let ((err-msg (format nil "[ERROR] ~A" eval-error)))
              (setf (shell-engine-last-eval-values engine) nil)
              (setf (shell-engine-last-status engine) :error)
              (push err-msg (shell-engine-output-history engine))
              :error)
            (progn
              (setf (shell-engine-last-eval-values engine) results)
              (setf (shell-engine-last-status engine) :submitted)
              (when results
                (dolist (val results)
                  (push (format nil "=> ~S" val) (shell-engine-output-history engine))))
              :submitted))))))

(defun evaluate-string (engine code-str)
  "Evaluate Common Lisp expression(s) or shell command(s) in CODE-STR.
If input begins with '(', ';', or '#', evaluates as pure Common Lisp.
Otherwise, treats input as a shell command line with implicit parentheses."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) code-str)))
    (cond
      ((zerop (length trimmed))
       (setf (shell-engine-last-eval-values engine) nil)
       (setf (shell-engine-last-stdout engine) "")
       (setf (shell-engine-last-stderr engine) "")
       (setf (shell-engine-last-error engine) nil)
       (setf (shell-engine-last-status engine) :empty)
       :empty)
      ((or (char= (char trimmed 0) #\()
           (char= (char trimmed 0) #\;)
           (char= (char trimmed 0) #\#))
       (evaluate-lisp-string engine trimmed))
      (t
       (evaluate-shell-line engine trimmed)))))

(defun submit-buffer (engine)
  "Submit the current buffer for evaluation:
- Records prompt line in output history.
- Adds line to history ring.
- Resets scratch buffer and clears input buffer.
- Performs captured evaluation."
  (let* ((line (buffer-text engine))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line))
         (prompt (engine-prompt engine)))
    ;; 1. Add input line with prompt to output history
    (push (format nil "~A~A" prompt line) (shell-engine-output-history engine))

    ;; 2. Push to history ring if non-empty and distinct from last command
    (when (> (length trimmed) 0)
      (let ((hist (shell-engine-history engine)))
        (unless (and hist (string= trimmed (first hist)))
          (push trimmed (shell-engine-history engine))
          (when (> (length (shell-engine-history engine)) (shell-engine-history-max engine))
            (setf (shell-engine-history engine)
                  (subseq (shell-engine-history engine) 0 (shell-engine-history-max engine)))))))

    ;; 3. Reset history navigation index and clear buffer
    (setf (shell-engine-history-index engine) nil)
    (setf (shell-engine-scratch-buffer engine) "")
    (setf (shell-engine-scratch-point engine) 0)
    (setf (fill-pointer (shell-engine-buffer engine)) 0)
    (setf (shell-engine-point engine) 0)

    ;; 4. Evaluate or handle empty submission
    (if (= (length trimmed) 0)
        (progn
          (setf (shell-engine-last-eval-values engine) nil)
          (setf (shell-engine-last-stdout engine) "")
          (setf (shell-engine-last-stderr engine) "")
          (setf (shell-engine-last-error engine) nil)
          (setf (shell-engine-last-status engine) :empty)
          :empty)
        (evaluate-string engine trimmed))))

(defun handle-enter (engine)
  "Pressing Enter checks if parentheses are balanced.
- If balanced: submits the form for evaluation.
- If unbalanced: inserts a newline so multi-line expressions can be completed."
  (let ((line (buffer-text engine)))
    (if (balanced-parentheses-p line)
        (submit-buffer engine)
        (progn
          (%buffer-insert-char engine #\Newline)
          (setf (shell-engine-last-status engine) :incomplete)
          :incomplete))))

(defun handle-key (engine key)
  "Dispatch simulated or live key event to ENGINE.
Accepts characters, ASCII control codes, or symbols (:left, :right, :home, :end,
:backspace, :delete, :up, :down, :enter, :ctrl-a, :ctrl-b, etc.)."
  (cond
    ;; Character key input
    ((characterp key)
     (let ((code (char-code key)))
       (cond
         ;; Enter / Newline / Return
         ((or (char= key #\Newline) (char= key #\Return) (= code 10) (= code 13))
          (handle-enter engine))
         ;; Backspace / Rubout
         ((or (char= key #\Backspace) (= code 127) (= code 8))
          (handle-backspace engine))
         ;; Tab
         ((char= key #\Tab)
          (handle-char engine #\Space)
          (handle-char engine #\Space))
         ;; ASCII Control characters
         ((= code 1)  (handle-home engine))          ; C-a
         ((= code 2)  (handle-left engine))          ; C-b
         ((= code 4)  (handle-delete engine))        ; C-d
         ((= code 5)  (handle-end engine))           ; C-e
         ((= code 6)  (handle-right engine))         ; C-f
         ((= code 11) (handle-kill-to-end engine))   ; C-k
         ((= code 12) (handle-clear-screen engine))  ; C-l
         ((= code 14) (handle-history-down engine))  ; C-n
         ((= code 16) (handle-history-up engine))    ; C-p
         ((= code 21) (handle-kill-line engine))     ; C-u
         ;; Standard character
         (t
          (handle-char engine key)))))

    ;; Symbol / Keyword key input
    ((symbolp key)
     (case (intern (string-upcase (symbol-name key)) :keyword)
       ((:left :c-b :ctrl-b :arrow-left)
        (handle-left engine))
       ((:right :c-f :ctrl-f :arrow-right)
        (handle-right engine))
       ((:home :c-a :ctrl-a)
        (handle-home engine))
       ((:end :c-e :ctrl-e)
        (handle-end engine))
       ((:backspace :rubout)
        (handle-backspace engine))
       ((:delete :c-d :ctrl-d)
        (handle-delete engine))
       ((:up :c-p :ctrl-p :arrow-up)
        (handle-history-up engine))
       ((:down :c-n :ctrl-n :arrow-down)
        (handle-history-down engine))
       ((:enter :return)
        (handle-enter engine))
       ((:c-k :ctrl-k)
        (handle-kill-to-end engine))
       ((:c-u :ctrl-u)
        (handle-kill-line engine))
       ((:c-l :ctrl-l)
        (handle-clear-screen engine))
       (t
        (error "Unknown key symbol: ~S" key))))

    (t
     (error "Unsupported key event type: ~S" key))))

(defun feed-input (engine input)
  "Feed INPUT into ENGINE. INPUT can be:
- A string (each character handled sequentially)
- A list of characters, strings, and key symbols
- A single key or character."
  (cond
    ((stringp input)
     (loop for char across input do
       (handle-key engine char)))
    ((listp input)
     (dolist (item input)
       (cond
         ((stringp item)
          (feed-input engine item))
         (t
          (handle-key engine item)))))
    (t
     (handle-key engine input))))

(defun render-prompt-and-buffer (engine)
  "Render the combined prompt and current buffer line."
  (format nil "~A~A" (shell-engine-prompt engine) (buffer-text engine)))

(defun output-history-lines (engine)
  "Return output history entries in chronological order (oldest first)."
  (reverse (shell-engine-output-history engine)))

(defun run-shell-tests ()
  "Run the shell engine test suite."
  (let ((test-file (merge-pathnames "tests/test-shell.lisp"
                                   (asdf:system-source-directory :lispos))))
    (if (probe-file test-file)
        (load test-file)
        (error "Test file not found: ~A" test-file))))
