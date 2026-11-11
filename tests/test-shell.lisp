;;;; tests/test-shell.lisp - Clean Headless Test Suite for LispOS REPL Shell Engine

(in-package :cl-user)

(defpackage :lispos.shell.test
  (:use :cl :lispos.shell)
  (:export #:run-all-tests))

(in-package :lispos.shell.test)

;;; ---------------------------------------------------------------------------
;;; Minimal Clean Test Harness
;;; ---------------------------------------------------------------------------

(defvar *test-passes* 0)
(defvar *test-failures* 0)
(defvar *current-group* "")

(defmacro assert-test (form &optional description)
  `(let ((res ,form))
     (if res
         (progn
           (incf *test-passes*)
           (format t "    [PASS] ~A~%" (or ,description ',form)))
         (progn
           (incf *test-failures*)
           (format t "    [FAIL] ~A (Evaluated: ~S)~%" (or ,description ',form) res)))))

(defmacro def-test-group (name &body body)
  `(progn
     (format t "~%=== [TEST SUITE] ~A ===~%" ,name)
     (setf *current-group* ,name)
     ,@body))

;;; ---------------------------------------------------------------------------
;;; Test Cases
;;; ---------------------------------------------------------------------------

(defun test-buffer-basics ()
  (def-test-group "1. Buffer Basics & Cursor Point"
    (let ((engine (make-shell-engine :prompt "λ > ")))
      (assert-test (string= (buffer-text engine) "") "Initial buffer is empty")
      (assert-test (= (buffer-point engine) 0) "Initial cursor point is 0")
      (assert-test (string= (engine-prompt engine) "λ > ") "Default prompt is λ > ")
      (assert-test (string= (render-prompt-and-buffer engine) "λ > ") "Render displays prompt only")

      ;; Feed simple text
      (feed-input engine "hello")
      (assert-test (string= (buffer-text engine) "hello") "Buffer contains 'hello'")
      (assert-test (= (buffer-point engine) 5) "Point is at end (5)")
      (assert-test (string= (render-prompt-and-buffer engine) "λ > hello") "Render displays prompt + hello")

      ;; Set buffer text manually
      (set-buffer-text engine "foo" 1)
      (assert-test (string= (buffer-text engine) "foo") "set-buffer-text updates buffer")
      (assert-test (= (buffer-point engine) 1) "set-buffer-text updates point")

      ;; Set buffer point clamp
      (set-buffer-point engine 100)
      (assert-test (= (buffer-point engine) 3) "Point clamped to buffer length")
      (set-buffer-point engine -5)
      (assert-test (= (buffer-point engine) 0) "Point clamped to 0"))))

(defun test-structural-auto-pairing ()
  (def-test-group "2. Structural Auto-Pairing for `(` and Step-Over `)`"
    (let ((engine (make-shell-engine)))
      ;; 1. Typing `(` inserts `()` and places point between them
      (handle-key engine #\()
      (assert-test (string= (buffer-text engine) "()") "Typing '(' inserts '()'")
      (assert-test (= (buffer-point engine) 1) "Cursor placed between parens (point = 1)")

      ;; 2. Typing inside the parens
      (feed-input engine "+ 1 2")
      (assert-test (string= (buffer-text engine) "(+ 1 2)") "Typed content inserted before closing paren")
      (assert-test (= (buffer-point engine) 6) "Point is right before closing paren")

      ;; 3. Typing `)` over existing `)` steps over it
      (handle-key engine #\))
      (assert-test (string= (buffer-text engine) "(+ 1 2)") "Typing ')' over existing ')' does NOT duplicate paren")
      (assert-test (= (buffer-point engine) 7) "Point stepped over closing paren (point = 7)")

      ;; 4. Typing `)` when not over `)` inserts `)` normally
      (handle-key engine #\))
      (assert-test (string= (buffer-text engine) "(+ 1 2))") "Typing ')' at end of buffer inserts ')'")
      (assert-test (= (buffer-point engine) 8) "Point advanced to 8"))))

(defun test-nested-auto-pairing ()
  (def-test-group "3. Nested Structural Auto-Pairing"
    (let ((engine (make-shell-engine)))
      ;; Typing `(+ 1 (` -> should give `(+ 1 ())` with cursor inside inner parens
      (feed-input engine "(+ 1 (")
      (assert-test (string= (buffer-text engine) "(+ 1 ())") "Nested '(' produces nested '()'")
      (assert-test (= (buffer-point engine) 6) "Cursor inside nested parens (point = 6)")

      ;; Add inner argument
      (feed-input engine "* 2 3")
      (assert-test (string= (buffer-text engine) "(+ 1 (* 2 3))") "Inner expression placed correctly")
      (assert-test (= (buffer-point engine) 11) "Cursor before first closing paren")

      ;; Step over inner paren
      (handle-key engine #\))
      (assert-test (= (buffer-point engine) 12) "Stepped over inner closing paren")
      (assert-test (string= (buffer-text engine) "(+ 1 (* 2 3))") "Inner paren not duplicated")

      ;; Step over outer paren
      (handle-key engine #\))
      (assert-test (= (buffer-point engine) 13) "Stepped over outer closing paren")
      (assert-test (string= (buffer-text engine) "(+ 1 (* 2 3))") "Outer paren not duplicated"))))

(defun test-backspace-and-delete ()
  (def-test-group "4. Backspace Auto-Pair Deletion & Delete Key"
    ;; 1. Backspace between `()` deletes both
    (let ((engine (make-shell-engine)))
      (handle-key engine #\()
      (assert-test (string= (buffer-text engine) "()") "Buffer is '()'")
      (assert-test (= (buffer-point engine) 1) "Point is 1")
      ;; Press backspace between `()`
      (handle-key engine :backspace)
      (assert-test (string= (buffer-text engine) "") "Backspace between '()' deletes BOTH parens")
      (assert-test (= (buffer-point engine) 0) "Point reset to 0"))

    ;; 2. Backspace when NOT between `()` deletes single character
    (let ((engine (make-shell-engine)))
      (feed-input engine "abc")
      (handle-key engine :backspace)
      (assert-test (string= (buffer-text engine) "ab") "Standard backspace deletes character before point")
      (assert-test (= (buffer-point engine) 2) "Point decremented by 1")

      ;; Backspace at point 0 does nothing
      (handle-key engine :home)
      (assert-test (= (buffer-point engine) 0) "Point at 0")
      (handle-key engine :backspace)
      (assert-test (string= (buffer-text engine) "ab") "Backspace at point 0 is a safe no-op")
      (assert-test (= (buffer-point engine) 0) "Point remains 0"))

    ;; 3. Delete key / C-d deletes character under point
    (let ((engine (make-shell-engine)))
      (feed-input engine "abcd")
      (handle-key engine :home)
      (handle-key engine :right) ; point = 1 ('b')
      (handle-key engine :delete)
      (assert-test (string= (buffer-text engine) "acd") "Delete removes character under point")
      (assert-test (= (buffer-point engine) 1) "Point remains unchanged after delete")

      ;; Delete with :c-d symbol
      (handle-key engine :c-d)
      (assert-test (string= (buffer-text engine) "ad") "C-d removes character under point")

      ;; Delete at end of buffer is safe no-op
      (handle-key engine :end)
      (handle-key engine :delete)
      (assert-test (string= (buffer-text engine) "ad") "Delete at end of buffer is safe no-op"))))

(defun test-cursor-navigation ()
  (def-test-group "5. Cursor Navigation (Arrows, Home, End, Emacs keys)"
    (let ((engine (make-shell-engine)))
      (feed-input engine "hello world")
      (assert-test (= (buffer-point engine) 11) "Point at end (11)")

      ;; Left arrow / C-b
      (handle-key engine :left)
      (assert-test (= (buffer-point engine) 10) ":left decrements point")
      (handle-key engine :c-b)
      (assert-test (= (buffer-point engine) 9) ":c-b decrements point")

      ;; Right arrow / C-f
      (handle-key engine :right)
      (assert-test (= (buffer-point engine) 10) ":right increments point")
      (handle-key engine :c-f)
      (assert-test (= (buffer-point engine) 11) ":c-f increments point")

      ;; Boundary check right
      (handle-key engine :right)
      (assert-test (= (buffer-point engine) 11) ":right clamped at buffer length")

      ;; Home / C-a
      (handle-key engine :home)
      (assert-test (= (buffer-point engine) 0) ":home moves to 0")
      (handle-key engine :end)
      (assert-test (= (buffer-point engine) 11) ":end moves to length")
      (handle-key engine :c-a)
      (assert-test (= (buffer-point engine) 0) ":c-a moves to 0")
      (handle-key engine :c-e)
      (assert-test (= (buffer-point engine) 11) ":c-e moves to length")

      ;; Boundary check left
      (handle-key engine :home)
      (handle-key engine :left)
      (assert-test (= (buffer-point engine) 0) ":left clamped at 0"))))

(defun test-history-ring ()
  (def-test-group "6. History Ring & Non-Destructive Scratch Retention"
    (let ((engine (make-shell-engine)))
      ;; Submit two commands
      (feed-input engine '("(+ 1 2)" :enter))
      (feed-input engine '("(* 10 20)" :enter))

      (assert-test (equal (engine-history engine) '("(* 10 20)" "(+ 1 2)"))
                   "History recorded submitted lines in most-recent-first order")

      ;; Type an unsubmitted scratch line
      (feed-input engine "scratch command")
      (assert-test (string= (buffer-text engine) "scratch command") "Scratch buffer typed")
      (assert-test (= (buffer-point engine) 15) "Scratch point is 15")

      ;; Press Up (C-p / :up): loads most recent command
      (handle-key engine :up)
      (assert-test (string= (buffer-text engine) "(* 10 20)") "Up loads most recent command")
      (assert-test (= (buffer-point engine) 9) "Point at end of history line")

      ;; Press Up again: loads older command
      (handle-key engine :c-p)
      (assert-test (string= (buffer-text engine) "(+ 1 2)") "Up again loads older command")

      ;; Press Up at oldest: stays at oldest
      (handle-key engine :up)
      (assert-test (string= (buffer-text engine) "(+ 1 2)") "Up at oldest command stays at oldest")

      ;; Edit the history line on screen (non-destructive test)
      (feed-input engine " ; edited")
      (assert-test (string= (buffer-text engine) "(+ 1 2) ; edited") "History line edited on screen")

      ;; Press Down (C-n / :down): loads newer command
      (handle-key engine :down)
      (assert-test (string= (buffer-text engine) "(* 10 20)") "Down loads newer command")

      ;; Press Down again: restores unsubmitted scratch line!
      (handle-key engine :c-n)
      (assert-test (string= (buffer-text engine) "scratch command") "Down restores unsubmitted scratch line")
      (assert-test (= (buffer-point engine) 15) "Scratch point restored")

      ;; Press Up twice to verify history was not mutated by the earlier edit
      (handle-key engine :up)
      (handle-key engine :up)
      (assert-test (string= (buffer-text engine) "(+ 1 2)")
                   "Original history entry remained untouched (non-destructive)"))))

(defun test-balanced-parentheses ()
  (def-test-group "7. Balanced Parentheses Checker"
    (assert-test (balanced-parentheses-p "(+ 1 2)") "Simple paren expression is balanced")
    (assert-test (not (balanced-parentheses-p "(+ 1 2")) "Unclosed paren is unbalanced")
    (assert-test (not (balanced-parentheses-p ")")) "Single close paren is unbalanced")
    (assert-test (not (balanced-parentheses-p "(()")) "Missing closing paren is unbalanced")
    (assert-test (not (balanced-parentheses-p "())")) "Extra closing paren is unbalanced")
    (assert-test (balanced-parentheses-p "(format t \"string with ) and ( inside\")")
                 "Parens inside string literals are ignored")
    (assert-test (not (balanced-parentheses-p "(format t \"unclosed string)"))
                 "Unclosed string literal is unbalanced")
    (assert-test (balanced-parentheses-p ";; comment with ( paren\n(+ 1 2)")
                 "Parens inside comments are ignored")
    (assert-test (balanced-parentheses-p "(list #\\() (list #\\))")
                 "Character literals #\\( and #\\) are ignored")
    (assert-test (balanced-parentheses-p "#| nested #| comments |# |# (+ 1 2)")
                 "Nested block comments #| ... |# are handled")
    (assert-test (not (balanced-parentheses-p "#| unclosed block (+ 1 2)"))
                 "Unclosed block comment is unbalanced")
    (assert-test (balanced-parentheses-p "|sym with (|")
                 "Paren in escaped symbol |...| is ignored")
    (assert-test (balanced-parentheses-p "") "Empty string is balanced")
    (assert-test (balanced-parentheses-p "   ") "Whitespace is balanced")))

(defun test-multiline-unbalanced-enter ()
  (def-test-group "8. Multi-Line Continuation on Unbalanced Enter"
    (let ((engine (make-shell-engine)))
      ;; Feed `(+ 10` and delete the auto-closed paren to test unbalanced entry
      (feed-input engine "(+ 10")
      (handle-key engine :delete)
      (assert-test (string= (buffer-text engine) "(+ 10") "Deleted closing paren, leaving incomplete expression")

      ;; Press Enter while unbalanced
      (let ((res (handle-key engine :enter)))
        (assert-test (eq res :incomplete) "Enter on unbalanced expression returns :incomplete")
        (assert-test (eq (engine-last-status engine) :incomplete) "Status is :incomplete")
        (assert-test (string= (buffer-text engine) (format nil "(+ 10~%"))
                     "Newline inserted into buffer for continuation"))

      ;; Complete the expression on next line
      (feed-input engine "   20)")
      (assert-test (string= (buffer-text engine) (format nil "(+ 10~%   20)"))
                   "Buffer contains complete multi-line expression")

      ;; Press Enter while balanced: should submit!
      (let ((res (handle-key engine :enter)))
        (assert-test (eq res :submitted) "Enter on balanced multi-line submits for evaluation")
        (assert-test (equal (engine-last-eval-values engine) '(30)) "Evaluated multi-line result is 30")
        (assert-test (string= (buffer-text engine) "") "Buffer cleared after submission")
        (assert-test (= (buffer-point engine) 0) "Point reset to 0 after submission")))))

(defun test-evaluation-and-output ()
  (def-test-group "9. Evaluation & Output Capture (stdout, stderr, values, errors)"
    ;; 1. Standard single value evaluation
    (let ((engine (make-shell-engine :prompt "λ > ")))
      (feed-input engine '("(+ 40 2)" :enter))
      (assert-test (eq (engine-last-status engine) :submitted) "Status is :submitted")
      (assert-test (equal (engine-last-eval-values engine) '(42)) "Evaluates (+ 40 2) to 42")
      (let ((history (output-history-lines engine)))
        (assert-test (find "λ > (+ 40 2)" history :test #'string=) "Prompt line recorded in output")
        (assert-test (find "=> 42" history :test #'string=) "Formatted return value recorded in output")))

    ;; 2. Multiple return values
    (let ((engine (make-shell-engine)))
      (feed-input engine '("(values 1 :two \"three\")" :enter))
      (assert-test (equal (engine-last-eval-values engine) '(1 :two "three"))
                   "Captures multiple return values")
      (let ((history (output-history-lines engine)))
        (assert-test (find "=> 1" history :test #'string=) "Return value 1 captured")
        (assert-test (find "=> :TWO" history :test #'string=) "Return value :TWO captured")
        (assert-test (find "=> \"three\"" history :test #'string=) "Return value \"three\" captured")))

    ;; 3. *standard-output* capture
    (let ((engine (make-shell-engine)))
      (feed-input engine '("(write-line \"Hello LispOS stdout!\")" :enter))
      (assert-test (search "Hello LispOS stdout!" (engine-last-stdout engine))
                   "Captures *standard-output* in last-stdout")
      (let ((history (output-history-lines engine)))
        (assert-test (find "Hello LispOS stdout!" history :test #'string=)
                     "Stdout line captured in output history")))

    ;; 4. *error-output* capture
    (let ((engine (make-shell-engine)))
      (feed-input engine '("(format *error-output* \"Warning from LispOS!~%\")" :enter))
      (assert-test (search "Warning from LispOS!" (engine-last-stderr engine))
                   "Captures *error-output* in last-stderr")
      (let ((history (output-history-lines engine)))
        (assert-test (find "[STDERR] Warning from LispOS!" history :test #'string=)
                     "Stderr line tagged and captured in output history")))

    ;; 5. Error handling immunity
    (let ((engine (make-shell-engine)))
      (feed-input engine '("(/ 1 0)" :enter))
      (assert-test (eq (engine-last-status engine) :error) "Division by zero sets status to :error")
      (assert-test (not (null (engine-last-error engine))) "Error condition object captured")
      (let ((history (output-history-lines engine)))
        (assert-test (some (lambda (line) (search "DIVISION-BY-ZERO" line)) history)
                     "Error description formatted into output history"))
      ;; Verify engine remains fully usable after an error
      (feed-input engine '("(+ 5 5)" :enter))
      (assert-test (equal (engine-last-eval-values engine) '(10))
                   "Engine immediately recovers and evaluates subsequent input"))

    ;; 6. Empty line submission advances prompt without evaluation
    (let ((engine (make-shell-engine :prompt "λ > ")))
      (feed-input engine '("" :enter))
      (assert-test (eq (engine-last-status engine) :empty) "Empty line sets status to :empty")
      (assert-test (null (engine-last-eval-values engine)) "No eval values for empty line")
      (assert-test (find "λ > " (output-history-lines engine) :test #'string=)
                   "Prompt recorded in output history"))))

(defun test-emacs-kill-shortcuts ()
  (def-test-group "10. Emacs Kill Shortcuts (C-k, C-u, C-l)"
    (let ((engine (make-shell-engine)))
      ;; C-k: kill to end of line
      (feed-input engine "hello world")
      (set-buffer-point engine 5)
      (handle-key engine :c-k)
      (assert-test (string= (buffer-text engine) "hello") "C-k kills text from point to end of line")
      (assert-test (= (buffer-point engine) 5) "Point remains 5")

      ;; C-u: kill line to start
      (feed-input engine " world")
      (set-buffer-point engine 5)
      (handle-key engine :c-u)
      (assert-test (string= (buffer-text engine) " world") "C-u kills text from start of line to point")
      (assert-test (= (buffer-point engine) 0) "Point reset to 0")

      ;; C-l: clear output history
      (feed-input engine '("(+ 1 1)" :enter))
      (assert-test (> (length (engine-output-history engine)) 0) "Output history has entries")
      (handle-key engine :c-l)
      (assert-test (null (engine-output-history engine)) "C-l clears output history"))))

(defun test-dynamic-prompt ()
  (def-test-group "11. Dynamic Prompt & CWD Integration"
    (let ((engine (make-shell-engine)))
      (assert-test (search "λ [" (engine-prompt engine)) "Default prompt contains λ [")
      (assert-test (search "] > " (engine-prompt engine)) "Default prompt contains ] > ")

      ;; Simulate cd to /tmp
      (let ((orig-cwd *default-pathname-defaults*))
        (unwind-protect
             (progn
               (feed-input engine '("cd /tmp" :enter))
               (assert-test (search "/tmp" (engine-prompt engine)) "Prompt updated with /tmp CWD"))
          (setf *default-pathname-defaults* orig-cwd)))

      ;; Custom *prompt-fn*
      (let ((*prompt-fn* (lambda (eng) (declare (ignore eng)) "custom-prompt> ")))
        (assert-test (string= (engine-prompt engine) "custom-prompt> ") "Custom prompt hook works")))))

(defun test-shell-tokenizer ()
  (def-test-group "12. Shell Line Tokenizer (Quotes & Flags)"
    (assert-test (equal (tokenize-shell-line "ls -la /System") '("ls" "-la" "/System"))
                 "Splits standard tokens")
    (assert-test (equal (tokenize-shell-line "echo \"Hello, World!\" 123") '("echo" "Hello, World!" "123"))
                 "Preserves double-quoted string with spaces")
    (assert-test (equal (tokenize-shell-line "cat 'path with spaces/file.txt'") '("cat" "path with spaces/file.txt"))
                 "Preserves single-quoted string")
    (assert-test (equal (tokenize-shell-line "   cd    ..   ") '("cd" ".."))
                 "Handles multiple spaces and padding")
    (assert-test (null (tokenize-shell-line "")) "Empty string returns NIL")
    (assert-test (null (tokenize-shell-line "    ")) "Whitespace returns NIL")))

(defun test-builtin-filesystem-commands ()
  (def-test-group "13. Built-in Filesystem Commands (ls, cd, pwd, cat, touch, mkdir, rm)"
    (let ((engine (make-shell-engine :prompt "λ > "))
          (orig-cwd *default-pathname-defaults*))
      (unwind-protect
           (progn
             ;; pwd
             (feed-input engine '("pwd" :enter))
             (assert-test (search (get-cwd-namestring) (engine-last-stdout engine)) "pwd prints current directory")

             ;; mkdir / touch / cat / rm
             (let ((test-dir (format nil "/tmp/lispos-test-~A" (get-universal-time))))
               (feed-input engine (list (format nil "mkdir -p ~A" test-dir) :enter))
               (assert-test (probe-file test-dir) "mkdir creates directory")

               ;; cd into test-dir
               (feed-input engine (list (format nil "cd ~A" test-dir) :enter))
               (assert-test (search "lispos-test" (get-cwd-namestring)) "cd changes working directory")

               ;; touch test.txt
               (feed-input engine '("touch test.txt" :enter))
               (assert-test (probe-file "test.txt") "touch creates file")

               ;; write content to test.txt and cat it
               (with-open-file (out "test.txt" :direction :output :if-exists :supersede)
                 (write-line "Pure Common Lisp Userland" out))
               (feed-input engine '("cat test.txt" :enter))
               (assert-test (search "Pure Common Lisp Userland" (engine-last-stdout engine))
                            "cat reads file contents")

               ;; ls
               (feed-input engine '("ls" :enter))
               (assert-test (search "test.txt" (engine-last-stdout engine)) "ls lists test.txt")

               ;; rm
               (feed-input engine '("rm test.txt" :enter))
               (assert-test (null (probe-file "test.txt")) "rm deletes file")

               ;; cd back out and cleanup
               (feed-input engine '("cd /tmp" :enter))
               (feed-input engine (list (format nil "rm -r ~A" test-dir) :enter))))
        (setf *default-pathname-defaults* orig-cwd)))))

(defun test-builtin-system-commands ()
  (def-test-group "14. Built-in System Commands (echo, ps, whoami, history, help)"
    (let ((engine (make-shell-engine :prompt "λ > ")))
      ;; echo
      (feed-input engine '("echo Hello LispOS Userland" :enter))
      (assert-test (search "Hello LispOS Userland" (engine-last-stdout engine)) "echo outputs text")

      (feed-input engine '("echo -n no-newline" :enter))
      (assert-test (string= (engine-last-stdout engine) "no-newline") "echo -n suppresses newline")

      ;; whoami
      (feed-input engine '("whoami" :enter))
      (assert-test (search "Admin" (engine-last-stdout engine)) "whoami outputs Admin")

      ;; ps
      (feed-input engine '("ps" :enter))
      (assert-test (search "main thread" (engine-last-stdout engine)) "ps lists active threads")

      ;; history
      (feed-input engine '("history" :enter))
      (assert-test (search "echo" (engine-last-stdout engine)) "history lists executed commands")

      ;; help
      (feed-input engine '("help" :enter))
      (assert-test (search "Command Mode" (engine-last-stdout engine)) "help displays command guide"))))

(defun test-dual-mode-evaluation ()
  (def-test-group "15. Dual-Mode Evaluation (Implicit Shell vs Explicit Lisp)"
    (let ((engine (make-shell-engine :prompt "λ > ")))
      ;; 1. Implicit parenthesis shell command
      (feed-input engine '("echo shell-mode-active" :enter))
      (assert-test (search "shell-mode-active" (engine-last-stdout engine))
                   "Shell command without parentheses executes in Command Mode")

      ;; 2. Explicit Lisp form
      (feed-input engine '("(+ 100 23)" :enter))
      (assert-test (equal (engine-last-eval-values engine) '(123))
                   "Parenthesized expression evaluates as Common Lisp")

      ;; 3. Unknown command reporting
      (feed-input engine '("nonexistent-command-xyz" :enter))
      (assert-test (search "Command not found: nonexistent-command-xyz" (engine-last-stderr engine))
                   "Unknown command reports friendly error with help hint")

      ;; 4. Extensibility: define-shell-command
      (define-shell-command test-greet (eng &optional (name "LispOS"))
        (declare (ignore eng))
        (format t "Welcome, ~A!~%" name))
      (feed-input engine '("test-greet Developer" :enter))
      (assert-test (search "Welcome, Developer!" (engine-last-stdout engine))
                   "define-shell-command successfully extends shell with new commands"))))

;;; ---------------------------------------------------------------------------
;;; Test Suite Runner
;;; ---------------------------------------------------------------------------

(defun run-all-tests ()
  (setf *test-passes* 0)
  (setf *test-failures* 0)
  (format t "~%================================================================~%")
  (format t " >>> LispOS Pure Interactive REPL Shell Engine Test Suite <<<~%")
  (format t "================================================================~%")

  (test-buffer-basics)
  (test-structural-auto-pairing)
  (test-nested-auto-pairing)
  (test-backspace-and-delete)
  (test-cursor-navigation)
  (test-history-ring)
  (test-balanced-parentheses)
  (test-multiline-unbalanced-enter)
  (test-evaluation-and-output)
  (test-emacs-kill-shortcuts)
  (test-dynamic-prompt)
  (test-shell-tokenizer)
  (test-builtin-filesystem-commands)
  (test-builtin-system-commands)
  (test-dual-mode-evaluation)

  (format t "~%================================================================~%")
  (format t " Test Results Summary: ~A Passed, ~A Failed.~%"
          *test-passes* *test-failures*)
  (format t "================================================================~%~%")
  (if (zerop *test-failures*)
      (progn
        (format t " SUCCESS: All LispOS shell engine tests passed successfully!~%")
        t)
      (progn
        (format t " FAILURE: Some tests failed.~%")
        (error "Test suite failed with ~A failure(s)." *test-failures*))))

;; If executed directly as a script
(run-all-tests)
