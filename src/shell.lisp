;;;; src/shell.lisp - Pure Common Lisp Interactive REPL Shell Engine
;;;; Independent of graphics rendering, providing structural editing,
;;;; paren auto-pairing, history ring, and evaluation capture.

(in-package :lispos.shell)

;;; ---------------------------------------------------------------------------
;;; Constants & Configuration
;;; ---------------------------------------------------------------------------

(defparameter +default-prompt+ "λ > "
  "Standard prompt prefix for the LispOS shell.")

(defvar *prompt-fn* nil
  "Custom prompt hook function (lambda (engine) ...).")

(defvar *default-prompt* :dynamic
  "Configurable default prompt string, :dynamic, or prompt function.")

(defun get-cwd-namestring ()
  "Return current working directory as a clean pathname string."
  (let ((raw (or (ignore-errors (namestring (truename *default-pathname-defaults*)))
                 (ignore-errors (namestring *default-pathname-defaults*))
                 "/")))
    (if (and (> (length raw) 1) (char= (char raw (1- (length raw))) #\/))
        (subseq raw 0 (1- (length raw)))
        raw)))

(defun default-lispos-prompt (&optional engine)
  "Render standard LispOS prompt showing CWD abbreviated with ~ for /Users/Admin."
  (declare (ignore engine))
  (let* ((cwd (get-cwd-namestring))
         (admin-home (ignore-errors (namestring (pathname lispos.paths:+admin-home+))))
         (clean-home (if (and admin-home (> (length admin-home) 1)
                              (char= (char admin-home (1- (length admin-home))) #\/))
                         (subseq admin-home 0 (1- (length admin-home)))
                         admin-home))
         (display-cwd (cond
                        ((string= cwd clean-home) "~")
                        ((and clean-home (search clean-home cwd) (= (search clean-home cwd) 0))
                         (concatenate 'string "~" (subseq cwd (length clean-home))))
                        (t cwd))))
    (format nil "λ [~A] > " display-cwd)))

(defun resolve-path (path-str)
  "Resolve PATH-STR relative to *default-pathname-defaults*, expanding ~ to Admin home."
  (let* ((clean-str (string-trim '(#\Space #\Tab #\Newline #\Return) (or path-str "")))
         (admin-home (ignore-errors (namestring (pathname lispos.paths:+admin-home+))))
         (home-dir (or admin-home "/Users/Admin/")))
    (cond
      ((or (zerop (length clean-str)) (string= clean-str "~"))
       (pathname home-dir))
      ((and (>= (length clean-str) 2) (string= (subseq clean-str 0 2) "~/"))
       (pathname (concatenate 'string home-dir (subseq clean-str 2))))
      ((char= (char clean-str 0) #\/)
       (pathname clean-str))
      (t
       (merge-pathnames clean-str *default-pathname-defaults*)))))

(defvar *shell-commands* (make-hash-table :test #'equalp)
  "Registry mapping shell command names to handler functions.")

(defmacro define-shell-command (name (engine &rest args) &body body)
  "Register a shell command NAME. The handler receives ENGINE and ARGS (list of strings)."
  (let ((cmd-str (string-downcase (string name))))
    `(progn
       (setf (gethash ,cmd-str *shell-commands*)
             (lambda (,engine ,@args)
               ,@body))
       ',name)))

(defun get-shell-command (cmd-name)
  "Look up shell command handler for CMD-NAME."
  (gethash (string-downcase (string cmd-name)) *shell-commands*))

(defun tokenize-shell-line (line)
  "Parse command line into token strings, respecting single and double quotes."
  (let ((tokens '())
        (current (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
        (in-quote nil)
        (len (length line)))
    (loop for i from 0 below len do
      (let ((c (char line i)))
        (cond
          ((and (or (char= c #\") (char= c #\'))
                (null in-quote))
           (setf in-quote c))
          ((and in-quote (char= c in-quote))
           (setf in-quote nil))
          ((and (null in-quote) (member c '(#\Space #\Tab #\Newline #\Return)))
           (when (> (length current) 0)
             (push (copy-seq current) tokens)
             (setf (fill-pointer current) 0)))
          (t
           (vector-push-extend c current)))))
    (when (> (length current) 0)
      (push (copy-seq current) tokens))
    (nreverse tokens)))

;;; ---------------------------------------------------------------------------
;;; Engine State Structure
;;; ---------------------------------------------------------------------------

(defstruct (shell-engine (:constructor %make-shell-engine))
  "State for the interactive REPL shell engine."
  (prompt :dynamic)
  (buffer (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)
   :type (vector character))
  (point 0 :type fixnum)
  (history '() :type list)
  (history-max 500 :type fixnum)
  (history-index nil) ; nil when not browsing history, else 0-indexed integer
  (scratch-buffer "" :type string)
  (scratch-point 0 :type fixnum)
  (output-history '() :type list) ; stored most-recent first
  (package (find-package :cl-user) :type package)
  (last-eval-values '() :type list)
  (last-stdout "" :type string)
  (last-stderr "" :type string)
  (last-error nil)
  (last-status nil :type symbol)) ; :submitted, :incomplete, :empty, :error

(defun make-shell-engine (&key (prompt *default-prompt*)
                               (package (find-package :cl-user))
                               (history-max 500))
  "Create and initialize a new REPL shell engine instance."
  (%make-shell-engine
   :prompt prompt
   :buffer (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)
   :point 0
   :history '()
   :history-max history-max
   :history-index nil
   :scratch-buffer ""
   :scratch-point 0
   :output-history '()
   :package (if (packagep package) package (find-package package))
   :last-eval-values '()
   :last-stdout ""
   :last-stderr ""
   :last-error nil
   :last-status nil))

;;; ---------------------------------------------------------------------------
;;; Buffer Accessors & Setters
;;; ---------------------------------------------------------------------------

(defun engine-buffer (engine)
  "Return current buffer content as a string."
  (copy-seq (shell-engine-buffer engine)))

(defun engine-point (engine)
  "Return cursor position point."
  (shell-engine-point engine))

(defun engine-prompt (engine)
  "Return resolved prompt string for ENGINE."
  (let ((p (shell-engine-prompt engine)))
    (cond
      ((functionp p) (funcall p engine))
      ((eq p :dynamic)
       (if *prompt-fn*
           (funcall *prompt-fn* engine)
           (default-lispos-prompt engine)))
      ((stringp p) p)
      (*prompt-fn* (funcall *prompt-fn* engine))
      (t (default-lispos-prompt engine)))))

(defun (setf engine-prompt) (val engine)
  (setf (shell-engine-prompt engine) val))

(defun engine-history (engine)
  (shell-engine-history engine))

(defun engine-history-index (engine)
  (shell-engine-history-index engine))

(defun engine-scratch-buffer (engine)
  (shell-engine-scratch-buffer engine))

(defun engine-scratch-point (engine)
  (shell-engine-scratch-point engine))

(defun engine-output-history (engine)
  (shell-engine-output-history engine))

(defun engine-package (engine)
  (shell-engine-package engine))

(defun engine-last-eval-values (engine)
  (shell-engine-last-eval-values engine))

(defun engine-last-stdout (engine)
  (shell-engine-last-stdout engine))

(defun engine-last-stderr (engine)
  (shell-engine-last-stderr engine))

(defun engine-last-error (engine)
  (shell-engine-last-error engine))

(defun engine-last-status (engine)
  (shell-engine-last-status engine))

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

;;; ---------------------------------------------------------------------------
;;; Low-Level Buffer Editing Primitives
;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; Structural Auto-Pairing & Character Insertion
;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; Deletion (Backspace & Delete)
;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; Cursor Navigation
;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; History Ring Navigation (Non-Destructive)
;;; ---------------------------------------------------------------------------

(defun %load-history-entry (engine text)
  "Load TEXT into buffer and place point at the end."
  (%set-buffer-from-string engine text)
  (setf (shell-engine-point engine) (length text)))

(defun handle-history-up (engine)
  "Navigate backwards in command history (Up arrow / C-p).
Non-destructive: retains current unsubmitted line as scratch buffer."
  (let ((history (shell-engine-history engine))
        (h-idx (shell-engine-history-index engine)))
    (when (null history)
      (return-from handle-history-up :no-history))
    (cond
      ;; Not currently browsing history: save scratch and load newest item (index 0)
      ((null h-idx)
       (setf (shell-engine-scratch-buffer engine) (buffer-text engine))
       (setf (shell-engine-scratch-point engine) (shell-engine-point engine))
       (setf (shell-engine-history-index engine) 0)
       (%load-history-entry engine (first history))
       :history-up)
      ;; Already browsing: move to next older item if available
      ((< (1+ h-idx) (length history))
       (incf (shell-engine-history-index engine))
       (%load-history-entry engine (nth (shell-engine-history-index engine) history))
       :history-up)
      (t
       :history-oldest))))

(defun handle-history-down (engine)
  "Navigate forwards in command history (Down arrow / C-n).
Restores scratch buffer when stepping past the newest command."
  (let ((history (shell-engine-history engine))
        (h-idx (shell-engine-history-index engine)))
    (cond
      ((null h-idx)
       :at-scratch)
      ((> h-idx 0)
       (decf (shell-engine-history-index engine))
       (%load-history-entry engine (nth (shell-engine-history-index engine) history))
       :history-down)
      ((= h-idx 0)
       ;; Step past index 0 back to unsubmitted scratch buffer
       (setf (shell-engine-history-index engine) nil)
       (let ((scratch (shell-engine-scratch-buffer engine))
             (s-pt (shell-engine-scratch-point engine)))
         (%set-buffer-from-string engine scratch)
         (setf (shell-engine-point engine) (min (length scratch) s-pt)))
       :restored-scratch))))

;;; ---------------------------------------------------------------------------
;;; Parentheses Balance Checker
;;; ---------------------------------------------------------------------------

(defun balanced-parentheses-p (text)
  "Verify whether parentheses, strings, comments, and character literals in TEXT
are fully balanced according to Common Lisp reader syntax.
Returns T if fully balanced, NIL otherwise."
  (let ((len (length text))
        (depth 0)
        (i 0)
        (in-string nil)
        (in-pipe nil)
        (block-comment-depth 0))
    (loop while (< i len) do
      (let ((c (char text i)))
        (cond
          ;; Inside string literal: " ... \" ... "
          (in-string
           (cond
             ((char= c #\\)
              (incf i)) ; skip escaped character
             ((char= c #\")
              (setf in-string nil))))

          ;; Inside symbol pipe quote: | ... \| ... |
          (in-pipe
           (cond
             ((char= c #\\)
              (incf i))
             ((char= c #\|)
              (setf in-pipe nil))))

          ;; Inside block comment: #| ... |# (supports nesting)
          ((> block-comment-depth 0)
           (cond
             ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\|))
              (incf block-comment-depth)
              (incf i))
             ((and (char= c #\|) (< (1+ i) len) (char= (char text (1+ i)) #\#))
              (decf block-comment-depth)
              (incf i))))

          ;; Normal top-level syntax
          (t
           (cond
             ;; String begin
             ((char= c #\")
              (setf in-string t))

             ;; Symbol pipe begin
             ((char= c #\|)
              (setf in-pipe t))

             ;; Single-line comment begin: ; ... newline
             ((char= c #\;)
              (loop while (and (< i len) (not (char= (char text i) #\Newline)))
                    do (incf i)))

             ;; Character literal #\
             ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\))
              (incf i 2)
              (when (< i len)
                (if (alphanumericp (char text i))
                    (loop while (and (< i len) (alphanumericp (char text i)))
                          do (incf i))
                    (incf i)))
              (decf i)) ; counterbalanced by loop's (incf i)

             ;; Block comment begin #|
             ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\|))
              (incf block-comment-depth)
              (incf i))

             ;; Opening paren
             ((char= c #\()
              (incf depth))

             ;; Closing paren
             ((char= c #\))
              (decf depth)
              (when (< depth 0)
                (return-from balanced-parentheses-p nil))))))
        (incf i)))
    (and (= depth 0)
         (not in-string)
         (not in-pipe)
         (= block-comment-depth 0))))

;;; ---------------------------------------------------------------------------
;;; Evaluation & Output Capture
;;; ---------------------------------------------------------------------------

(defun split-lines (str)
  "Split string STR into a list of lines without trailing newlines."
  (let ((lines '())
        (start 0)
        (len (length str)))
    (loop for i from 0 below len do
      (when (char= (char str i) #\Newline)
        (push (subseq str start i) lines)
        (setf start (1+ i))))
    (when (< start len)
      (push (subseq str start len) lines))
    (nreverse lines)))

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

;;; ---------------------------------------------------------------------------
;;; High-Level Key Event Dispatcher
;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; Display & Output Helpers
;;; ---------------------------------------------------------------------------

(defun render-prompt-and-buffer (engine)
  "Render the combined prompt and current buffer line."
  (format nil "~A~A" (shell-engine-prompt engine) (buffer-text engine)))

(defun output-history-lines (engine)
  "Return output history entries in chronological order (oldest first)."
  (reverse (shell-engine-output-history engine)))

;;; ---------------------------------------------------------------------------
;;; Headless Self-Test Runner Interface
;;; ---------------------------------------------------------------------------

(defun run-shell-tests ()
  "Run the shell engine test suite."
  (let ((test-file (merge-pathnames "tests/test-shell.lisp"
                                   (asdf:system-source-directory :lispos))))
    (if (probe-file test-file)
        (load test-file)
        (error "Test file not found: ~A" test-file))))
