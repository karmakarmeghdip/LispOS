;;;; tests/test-buffer.lisp - Polymorphic CLOS Buffer & Helix Modal Editor Test Suite

(in-package :cl-user)

(defpackage :lispos.buffer.test
  (:use :cl :lispos.buffer)
  (:export #:run-all-tests))

(in-package :lispos.buffer.test)

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
;;; 1. Base Buffer Class & Registry
;;; ---------------------------------------------------------------------------

(defun test-buffer-registry ()
  (def-test-group "1. Buffer Protocol & Registry Lifecycle"
    (setf *buffers* nil
          *current-buffer* nil)

    (let ((sh (make-instance 'shell-buffer :name "*test-shell*"))
          (ed (make-instance 'editor-buffer :name "*test-editor*")))
      (assert-test (buffer-p sh) "shell-buffer is a buffer")
      (assert-test (buffer-p ed) "editor-buffer is a buffer")
      (assert-test (string= (buffer-name sh) "*test-shell*") "shell buffer name matches")
      (assert-test (string= (buffer-name ed) "*test-editor*") "editor buffer name matches")
      (assert-test (not (buffer-modified-p ed)) "editor initially unmodified")
      (assert-test (not (buffer-read-only-p ed)) "editor initially writable")

      (register-buffer sh)
      (register-buffer ed)
      (assert-test (= (length (list-buffers)) 2) "list-buffers contains 2 buffers")
      (assert-test (eq *current-buffer* sh) "*current-buffer* defaults to first registered")
      (assert-test (eq (find-buffer "*test-shell*") sh) "find-buffer finds *test-shell*")
      (assert-test (eq (find-buffer "*test-editor*") ed) "find-buffer finds *test-editor*")

      (kill-buffer ed)
      (assert-test (= (length (list-buffers)) 1) "kill-buffer removed editor")
      (assert-test (null (find-buffer "*test-editor*")) "editor no longer found in registry")
      (assert-test (eq *current-buffer* sh) "current buffer stays shell"))))

;;; ---------------------------------------------------------------------------
;;; 2. Shell Buffer Protocol
;;; ---------------------------------------------------------------------------

(defun test-shell-buffer-protocol ()
  (def-test-group "2. Shell Buffer Protocol"
    (let ((sh (make-instance 'shell-buffer :name "*shell*")))
      (assert-test (string= (buffer-mode-name sh) "INS") "buffer-mode-name is INS")
      (assert-test (eq (buffer-mode sh) :shell) "buffer-mode is :shell")
      (assert-test (eq (buffer-mode-icon sh) (code-char #xF448)) "buffer-mode-icon is pencil in insert mode")
      (assert-test (consp (buffer-content-lines sh)) "buffer-content-lines returns list")

      (multiple-value-bind (r c) (buffer-cursor-position sh)
        (assert-test (>= r 0) "shell cursor row >= 0")
        (assert-test (>= c 0) "shell cursor col >= 0"))

      (buffer-handle-key sh #\a :char #\a)
      (buffer-handle-key sh #\b :char #\b)
      (let* ((lines (buffer-content-lines sh))
             (last-line (first (last lines))))
        (assert-test (search "ab" last-line) "shell engine processed keys into current prompt line")))))

;;; ---------------------------------------------------------------------------
;;; 3. Helix Modal Engine: Insert Mode & Character Typing
;;; ---------------------------------------------------------------------------

(defun test-helix-insert-mode ()
  (def-test-group "3. Helix Modal Engine: Insert Mode & Typing"
    (let ((ed (make-instance 'editor-buffer :name "*test*")))
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "editor starts in :normal mode")
      (assert-test (string= (buffer-mode-name ed) "NOR") "mode name is NOR")

      (buffer-handle-key ed #\i :char #\i)
      (assert-test (eq (editor-buffer-helix-mode ed) :insert) "pressing 'i' transitions to :insert")
      (assert-test (string= (buffer-mode-name ed) "INS") "mode name is INS")

      (buffer-handle-key ed #\H :char #\H)
      (buffer-handle-key ed #\e :char #\e)
      (buffer-handle-key ed #\l :char #\l)
      (buffer-handle-key ed #\l :char #\l)
      (buffer-handle-key ed #\o :char #\o)
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "Hello") "buffer line contains 'Hello'")
      (assert-test (buffer-modified-p ed) "buffer marked modified after typing")

      (buffer-handle-key ed :enter)
      (buffer-handle-key ed #\W :char #\W)
      (buffer-handle-key ed #\o :char #\o)
      (buffer-handle-key ed #\r :char #\r)
      (buffer-handle-key ed #\l :char #\l)
      (buffer-handle-key ed #\d :char #\d)
      (let ((lines (buffer-content-lines ed)))
        (assert-test (= (length lines) 2) "buffer now has 2 lines")
        (assert-test (string= (first lines) "Hello") "line 1 is 'Hello'")
        (assert-test (string= (second lines) "World") "line 2 is 'World'"))

      (buffer-handle-key ed :backspace)
      (let ((lines (buffer-content-lines ed)))
        (assert-test (string= (second lines) "Worl") "backspace deleted 'd'"))

      (buffer-handle-key ed :escape)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "escape transitions back to :normal mode")
      (assert-test (string= (buffer-mode-name ed) "NOR") "mode name is NOR"))))

;;; ---------------------------------------------------------------------------
;;; 4. Helix Normal Mode Motions & Editing
;;; ---------------------------------------------------------------------------

(defun test-helix-normal-motions ()
  (def-test-group "4. Helix Normal Mode Motions & Editing"
    (let ((ed (make-instance 'editor-buffer :name "*test*")))
      (setf (editor-buffer-lines ed)
            (make-array 2 :adjustable t :fill-pointer 2
                        :initial-contents (list "The quick brown fox" "jumps over the dog"))
            (editor-buffer-cursor-row ed) 0
            (editor-buffer-cursor-col ed) 0)

      (multiple-value-bind (r c) (buffer-cursor-position ed)
        (assert-test (and (= r 0) (= c 0)) "cursor initially at 0,0"))

      (buffer-handle-key ed #\l :char #\l)
      (assert-test (= (editor-buffer-cursor-col ed) 1) "'l' advances col to 1")

      (buffer-handle-key ed #\h :char #\h)
      (assert-test (= (editor-buffer-cursor-col ed) 0) "'h' moves col back to 0")

      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 4) "'w' jumps to next word 'quick' (col 4)")
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "'w' stays in :normal mode")
      (assert-test (= (editor-buffer-selection-anchor-col ed) 0) "'w' sets selection anchor to previous col 0")
      (assert-test (= (editor-buffer-selection-end-col ed) 3) "'w' sets head-exclusive end col to 3")

      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 10) "'w' jumps to 'brown' (col 10)")
      (assert-test (= (editor-buffer-selection-anchor-col ed) 4) "'w' replaces selection anchor with previous col 4")
      (assert-test (= (editor-buffer-selection-end-col ed) 9) "'w' sets head-exclusive end col to 9")

      (buffer-handle-key ed #\b :char #\b)
      (assert-test (= (editor-buffer-cursor-col ed) 4) "'b' jumps back to 'quick' (col 4)")
      (assert-test (= (editor-buffer-selection-anchor-col ed) 4) "'b' sets selection anchor to col 4")
      (assert-test (= (editor-buffer-selection-end-col ed) 9) "'b' sets selection end to col 9 (excluding col 10)")

      ;; Single-step motion in normal mode cancels selection
      (buffer-handle-key ed #\l :char #\l)
      (assert-test (= (editor-buffer-cursor-col ed) 5) "'l' advances col to 5")
      (assert-test (null (editor-buffer-selection-anchor-row ed)) "single-step motion 'l' cancels normal mode selection")

      (buffer-handle-key ed #\$ :char #\$)
      (assert-test (= (editor-buffer-cursor-col ed) 18) "'$' jumps to end of line")

      (buffer-handle-key ed #\0 :char #\0)
      (assert-test (= (editor-buffer-cursor-col ed) 0) "'0' jumps to col 0")

      (buffer-handle-key ed #\j :char #\j)
      (assert-test (= (editor-buffer-cursor-row ed) 1) "'j' moves down to row 1")
      (assert-test (null (editor-buffer-selection-anchor-row ed)) "'j' cancels selection")

      (buffer-handle-key ed #\k :char #\k)
      (assert-test (= (editor-buffer-cursor-row ed) 0) "'k' moves up to row 0")

      ;; In Helix: 'd' immediately deletes char under cursor (1-char selection)
      (buffer-handle-key ed #\d :char #\d)
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "he quick brown fox") "'d' deletes char under cursor")

      ;; In Helix: 'x' selects current line in Normal mode ('NOR')
      (buffer-handle-key ed #\x :char #\x)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "'x' selects current line while remaining in :normal mode")
      (assert-test (string= (buffer-mode-name ed) "NOR") "mode name remains NOR")
      (assert-test (= (editor-buffer-selection-anchor-row ed) 0) "line anchor row is 0")

      ;; Pressing 'x' a second time extends the line selection across multiple lines
      (buffer-handle-key ed #\x :char #\x)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "second 'x' stays in :normal mode")
      (assert-test (= (editor-buffer-selection-anchor-row ed) 0) "line anchor remains at 0")
      (assert-test (= (editor-buffer-cursor-row ed) 1) "cursor extended to row 1")

      ;; Arrow key or motion in normal mode cancels multi-line selection
      (buffer-handle-key ed :up)
      (assert-test (null (editor-buffer-selection-anchor-row ed)) "arrow key cancelled multi-line selection in normal mode")
      (assert-test (= (editor-buffer-cursor-row ed) 0) "cursor moved to row 0")

      ;; 'x' selects line, then 'd' deletes selection ('xd' deletes line)
      (buffer-handle-key ed #\x :char #\x)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "'x' selected line 0")
      (buffer-handle-key ed #\d :char #\d)
      (assert-test (= (length (editor-buffer-lines ed)) 1) "'xd' deleted current line")
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "jumps over the dog") "remaining line is second line")
      (assert-test (string= (editor-buffer-clipboard ed) "he quick brown fox") "'xd' saved deleted line to clipboard")

      (buffer-handle-key ed #\p :char #\p)
      (assert-test (= (length (editor-buffer-lines ed)) 2) "'p' pasted line below")
      (assert-test (string= (aref (editor-buffer-lines ed) 1) "he quick brown fox") "pasted content matches clipboard"))))

;;; ---------------------------------------------------------------------------
;;; 5. Helix Command Mode (:w, :q, :wq) & File Persistence
;;; ---------------------------------------------------------------------------

(defun test-helix-command-mode-and-persistence ()
  (def-test-group "5. Helix Command Mode & Disk Persistence"
    (let* ((tmp-path (format nil "/tmp/lispos-test-file-~D.txt" (get-universal-time)))
           (fb (open-file-in-buffer tmp-path)))
      (assert-test (typep fb 'file-buffer) "open-file-in-buffer returns file-buffer instance")
      (assert-test (string= (buffer-file-path fb) tmp-path) "buffer-file-path matches")

      (buffer-handle-key fb #\i :char #\i)
      (loop for ch across "(defun test () 42)" do
        (buffer-handle-key fb ch :char ch))
      (buffer-handle-key fb :escape)
      (assert-test (buffer-modified-p fb) "buffer marked modified")

      (buffer-handle-key fb #\: :char #\:)
      (assert-test (eq (editor-buffer-helix-mode fb) :command) "':' enters command mode")
      (assert-test (string= (buffer-mode-name fb) "CMD") "mode name is CMD")

      (buffer-handle-key fb #\w :char #\w)
      (assert-test (string= (editor-buffer-command-input fb) "w") "command input contains 'w'")
      (buffer-handle-key fb :return)

      (assert-test (eq (editor-buffer-helix-mode fb) :normal) "command execution returns to :normal mode")
      (assert-test (not (buffer-modified-p fb)) "buffer marked unmodified after save")

      (assert-test (probe-file tmp-path) "file exists on disk")
      (let ((content (uiop:read-file-string tmp-path)))
        (assert-test (search "(defun test () 42)" content) "saved file content contains typed text"))

      (buffer-handle-key fb #\: :char #\:)
      (buffer-handle-key fb #\q :char #\q)
      (buffer-handle-key fb :return)
      (assert-test (null (find-buffer (buffer-name fb))) ":q removed buffer from registry")

      (delete-file tmp-path))))

;;; ---------------------------------------------------------------------------
;;; 6. Helix Selection Mode (v, x, X, %, ;, Alt-;, motions, y, d, c)
;;; ---------------------------------------------------------------------------

(defun test-helix-selection-mode ()
  (def-test-group "6. Helix Selection Mode & Visual Operations"
    (let ((ed (make-instance 'editor-buffer :name "*test-sel*")))
      (setf (editor-buffer-lines ed)
            (make-array 2 :adjustable t :fill-pointer 2
                        :initial-contents (list "First line text" "Second line text"))
            (editor-buffer-cursor-row ed) 0
            (editor-buffer-cursor-col ed) 0)

      ;; Enter visual selection mode with 'v'
      (buffer-handle-key ed #\v :char #\v)
      (assert-test (eq (editor-buffer-helix-mode ed) :select) "'v' enters :select mode")
      (assert-test (string= (buffer-mode-name ed) "SEL") "mode name is SEL")
      (assert-test (= (editor-buffer-selection-anchor-row ed) 0) "selection anchor row is 0")
      (assert-test (= (editor-buffer-selection-anchor-col ed) 0) "selection anchor col is 0")

      ;; Move cursor with 'w'
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 6) "'w' advances cursor in select mode")

      ;; Yank selection with 'y'
      (buffer-handle-key ed #\y :char #\y)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "'y' returns to :normal mode")
      (assert-test (null (editor-buffer-selection-anchor-row ed)) "anchor cleared after yank")
      (assert-test (string= (editor-buffer-clipboard ed) "First l") "yanked exact selected text into clipboard")

      ;; Helix 'x' line selection in normal mode stays in :normal mode
      (buffer-handle-key ed #\x :char #\x)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "'x' stays in :normal mode")
      (assert-test (= (editor-buffer-selection-anchor-col ed) 0) "anchor col is 0 for line select")

      ;; Flip selection with Alt-;
      (buffer-handle-key ed :alt-semicolon)
      (assert-test (= (editor-buffer-cursor-col ed) 0) "Alt-; flipped cursor to start of line")

      ;; Collapse selection with ';'
      (buffer-handle-key ed #\; :char #\;)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "';' collapsed selection to normal mode")

      ;; Helix '%' select all in normal mode
      (buffer-handle-key ed #\% :char #\%)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "'%' selected entire buffer in normal mode")
      (assert-test (not (null (editor-buffer-selection-anchor-row ed))) "anchor set for '%'")

      ;; Delete entire selection with 'd'
      (buffer-handle-key ed #\d :char #\d)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "'d' on selection returns to :normal")
      (assert-test (= (length (editor-buffer-lines ed)) 1) "entire buffer cleared")
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "") "buffer line is empty")

      ;; 'v' toggle in select mode returns to normal mode
      (buffer-handle-key ed #\v :char #\v)
      (assert-test (eq (editor-buffer-helix-mode ed) :select) "'v' enters :select mode")
      (buffer-handle-key ed #\v :char #\v)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "'v' in select mode toggles back to :normal mode"))))

;;; ---------------------------------------------------------------------------
;;; 7. Undo & Redo Subsystem (u, U)
;;; ---------------------------------------------------------------------------

(defun test-helix-undo-redo ()
  (def-test-group "7. Undo & Redo Subsystem"
    (let ((ed (make-instance 'editor-buffer :name "*test-undo*")))
      (setf (editor-buffer-lines ed)
            (make-array 1 :adjustable t :fill-pointer 1
                        :initial-contents (list "Initial state"))
            (editor-buffer-cursor-row ed) 0
            (editor-buffer-cursor-col ed) 0)

      ;; Delete 'I' with 'd'
      (buffer-handle-key ed #\d :char #\d)
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "nitial state") "state modified")

      ;; Undo with 'u'
      (buffer-handle-key ed #\u :char #\u)
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "Initial state") "'u' restored initial state")

      ;; Redo with 'U'
      (buffer-handle-key ed #\U :char #\U)
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "nitial state") "'U' re-applied modification"))))

;;; ---------------------------------------------------------------------------
;;; 8. Helix Space Leader Keybindings (SPC w, SPC q, SPC s)
;;; ---------------------------------------------------------------------------

(defun test-helix-space-leader ()
  (def-test-group "8. Helix Space Leader Keybindings"
    (let* ((tmp-path (format nil "/tmp/lispos-leader-test-~D.txt" (get-universal-time)))
           (fb (open-file-in-buffer tmp-path)))
      ;; Press Space -> activates leader mode
      (buffer-handle-key fb #\Space :char #\Space)
      (assert-test (editor-buffer-leader-active-p fb) "Space activates leader mode")
      (assert-test (search "[SPC]" (buffer-modeline-details fb)) "modeline displays leader guide")

      ;; Press 'w' -> saves buffer
      (buffer-handle-key fb #\w :char #\w)
      (assert-test (not (editor-buffer-leader-active-p fb)) "leader mode cleared after action")
      (assert-test (probe-file tmp-path) "SPC w saved file to disk")

      ;; Press Space -> 'q' quits buffer
      (buffer-handle-key fb #\Space :char #\Space)
      (buffer-handle-key fb #\q :char #\q)
      (assert-test (null (find-buffer (buffer-name fb))) "SPC q killed buffer")

      (delete-file tmp-path))))

;;; ---------------------------------------------------------------------------
;;; 9. Scratch Buffer Lifecycle & Transition to File
;;; ---------------------------------------------------------------------------

(defun test-scratch-buffer-flow ()
  (def-test-group "9. Scratch Buffer Flow & Conversion"
    (let* ((tmp-scratch-out (format nil "/tmp/lispos-scratch-~D.txt" (get-universal-time)))
           (sc (create-scratch-buffer)))
      (assert-test (typep sc 'scratch-buffer) "create-scratch-buffer returns scratch-buffer")
      (assert-test (string= (buffer-name sc) "*scratch*") "buffer name is *scratch*")
      (assert-test (string= (buffer-mode-name sc) "NOR") "starts in NOR mode")

      ;; Save scratch to a file via :w <path>
      (save-buffer sc tmp-scratch-out)
      (assert-test (probe-file tmp-scratch-out) "scratch buffer saved to disk")
      (assert-test (typep sc 'file-buffer) "scratch-buffer converted to file-buffer")
      (assert-test (string= (buffer-file-path sc) tmp-scratch-out) "file-path set correctly")
      (assert-test (string= (buffer-name sc) (file-namestring tmp-scratch-out)) "buffer renamed to filename")

      (kill-buffer sc)
      (delete-file tmp-scratch-out))))

;;; ---------------------------------------------------------------------------
;;; 10. Shell 'edit' and 'scratch' Commands
;;; ---------------------------------------------------------------------------

(defun test-shell-commands ()
  (def-test-group "10. Shell 'edit' and 'scratch' Built-ins"
    (let ((eng (lispos.shell:make-shell-engine))
          (test-file (format nil "/tmp/shell-edit-~D.txt" (get-universal-time))))
      ;; Test edit with file path
      (lispos.shell:feed-input eng (list (format nil "edit ~A" test-file) :enter))
      (let ((fb (find-if (lambda (b)
                           (and (typep b 'file-buffer)
                                (string= (buffer-file-path b) test-file)))
                         (list-buffers))))
        (assert-test (not (null fb)) "edit command created and registered file-buffer")
        (when fb (kill-buffer fb)))

      ;; Test scratch command
      (lispos.shell:feed-input eng '("scratch" :enter))
      (let ((sc (find-buffer "*scratch*")))
        (assert-test (not (null sc)) "scratch command created and registered *scratch* buffer")))))

;;; ---------------------------------------------------------------------------
;;; 11. Helix Match, Goto, and Transformations
;;; ---------------------------------------------------------------------------

(defun test-helix-advanced-features ()
  (def-test-group "11. Helix Match, Goto, and Transformations"
    (let ((ed (make-instance 'editor-buffer :name "*test-adv*")))
      (setf (editor-buffer-lines ed)
            (make-array 3 :adjustable t :fill-pointer 3
                        :initial-contents (list "  (hello \"world\")" "second line" "third line"))
            (editor-buffer-cursor-row ed) 0
            (editor-buffer-cursor-col ed) 0)

      ;; 1. Goto: gh goes to line start (0), gl goes to line end
      (buffer-handle-key ed #\g :char #\g)
      (buffer-handle-key ed #\l :char #\l)
      (assert-test (= (editor-buffer-cursor-col ed) (1- (length "  (hello \"world\")"))) "gl jumps to line end")

      (buffer-handle-key ed #\g :char #\g)
      (buffer-handle-key ed #\h :char #\h)
      (assert-test (= (editor-buffer-cursor-col ed) 0) "gh jumps to line start 0")

      ;; 2. Match bracket: mm jumps to matching paren
      (buffer-handle-key ed #\m :char #\m)
      (buffer-handle-key ed #\m :char #\m)
      (assert-test (char= (char (aref (editor-buffer-lines ed) 0) (editor-buffer-cursor-col ed)) #\))
                   "mm jumps to matching closing paren")

      ;; 3. Insert transitions: I at line start, A at line end
      (buffer-handle-key ed :escape)
      (buffer-handle-key ed #\I :char #\I)
      (assert-test (eq (editor-buffer-helix-mode ed) :insert) "'I' enters insert mode")
      (assert-test (= (editor-buffer-cursor-col ed) 2) "'I' positioned at first non-whitespace")
      (buffer-handle-key ed :escape)

      (buffer-handle-key ed #\A :char #\A)
      (assert-test (eq (editor-buffer-helix-mode ed) :insert) "'A' enters insert mode")
      (assert-test (= (editor-buffer-cursor-col ed) (length "  (hello \"world\")")) "'A' positioned at line end")
      (buffer-handle-key ed :escape)

      ;; 4. Text objects: miw selects inside word
      (setf (editor-buffer-cursor-row ed) 1
            (editor-buffer-cursor-col ed) 1) ; on "second"
      (buffer-handle-key ed #\m :char #\m)
      (buffer-handle-key ed #\i :char #\i)
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "miw in normal mode stays in :normal mode")
      (assert-test (string= (lispos.buffer::%extract-selection-text ed) "second") "miw selected word 'second'")

      ;; 5. Transformations: ~ toggles case
      (buffer-handle-key ed #\~ :char #\~)
      (assert-test (search "SECOND" (aref (editor-buffer-lines ed) 1)) "~ toggled case to uppercase")

      ;; 6. Indent > and Unindent <
      (buffer-handle-key ed #\> :char #\>)
      (assert-test (search "  SECOND" (aref (editor-buffer-lines ed) 1)) "> indented line")
      (buffer-handle-key ed #\< :char #\<)
      (assert-test (search "SECOND" (aref (editor-buffer-lines ed) 1)) "< unindented line")

      ;; 7. Join lines J
      (buffer-handle-key ed #\J :char #\J)
      (assert-test (= (length (editor-buffer-lines ed)) 2) "J joined 2 lines into 1")
      (assert-test (search "third line" (aref (editor-buffer-lines ed) 1)) "joined content includes next line"))))

;;; ---------------------------------------------------------------------------
;;; 12. Word Motions with Symbols & Head-Exclusive Deletion
;;; ---------------------------------------------------------------------------

(defun test-word-motions-and-del-discrepancies ()
  (def-test-group "12. Word Motions with Symbols & Head-Exclusive Deletion"
    ;; 1. Hopping over symbols and punctuation: (hello "world") (+ 1 2)
    (let ((ed (make-instance 'editor-buffer :name "*test*")))
      (setf (editor-buffer-lines ed)
            (make-array 1 :adjustable t :fill-pointer 1
                        :initial-contents (list "(hello \"world\") (+ 1 2)"))
            (editor-buffer-cursor-row ed) 0
            (editor-buffer-cursor-col ed) 0)

      ;; Start at col 0 '('
      (assert-test (= (editor-buffer-cursor-col ed) 0) "start at col 0 '('")
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 1) "'w' from '(' hops to 'h' (col 1)")
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 7) "'w' from 'hello' hops to '\"' (col 7)")
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 8) "'w' from '\"' hops to 'w' (col 8)")
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 13) "'w' from 'world' hops to '\")' (col 13)")
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 16) "'w' from '\")' hops to '(+' (col 16)")
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 19) "'w' from '(+' hops to '1' (col 19)")
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 21) "'w' from '1' hops to '2' (col 21)")
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 22) "'w' from '2' hops to ')' (col 22)")

      ;; 2. Backward 'b' motion across symbols
      (buffer-handle-key ed #\b :char #\b)
      (assert-test (= (editor-buffer-cursor-col ed) 21) "'b' from ')' hops to '2' (col 21)")
      (buffer-handle-key ed #\b :char #\b)
      (assert-test (= (editor-buffer-cursor-col ed) 19) "'b' from '2' hops to '1' (col 19)")
      (buffer-handle-key ed #\b :char #\b)
      (assert-test (= (editor-buffer-cursor-col ed) 16) "'b' from '1' hops to '(+' (col 16)")
      (buffer-handle-key ed #\b :char #\b)
      (assert-test (= (editor-buffer-cursor-col ed) 13) "'b' from '(+' hops to '\")' (col 13)")

      ;; 3. Deletion with 'd' does NOT delete first character of next word!
      (setf (editor-buffer-lines ed)
            (make-array 1 :adjustable t :fill-pointer 1
                        :initial-contents (list "hello world"))
            (editor-buffer-cursor-row ed) 0
            (editor-buffer-cursor-col ed) 0)
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 6) "cursor is on 'w' of world (col 6)")
      (assert-test (= (editor-buffer-selection-end-col ed) 5) "selection end col is 5 (space before 'w')")
      (buffer-handle-key ed #\d :char #\d)
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "world") "'wd' deletes 'hello ' preserving 'world'")
      (assert-test (= (editor-buffer-cursor-col ed) 0) "cursor is at col 0 ('w' of 'world')")

      ;; 4. Change with 'c' deletes word and enters insert mode
      (setf (editor-buffer-lines ed)
            (make-array 1 :adjustable t :fill-pointer 1
                        :initial-contents (list "first second"))
            (editor-buffer-cursor-row ed) 0
            (editor-buffer-cursor-col ed) 0)
      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 6) "cursor at 'second'")
      (buffer-handle-key ed #\c :char #\c)
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "second") "'wc' deleted 'first ' leaving 'second'")
      (assert-test (eq (editor-buffer-helix-mode ed) :insert) "'c' entered :insert mode"))))

;;; ---------------------------------------------------------------------------
;;; 13. Terminal Shell Buffer Helix Modal Scrollback & Clipboard
;;; ---------------------------------------------------------------------------

(defun test-shell-buffer-helix-scrollback ()
  (def-test-group "13. Terminal Shell Buffer Helix Modal Scrollback & Clipboard"
    (let ((sh (make-instance 'shell-buffer :name "*test-shell*")))
      ;; Initial state: starts in :insert mode
      (assert-test (eq (editor-buffer-helix-mode sh) :insert) "shell-buffer starts in :insert mode")
      (assert-test (string= (buffer-mode-name sh) "INS") "mode name is INS")
      (assert-test (eq (buffer-mode-icon sh) (code-char #xF448)) "mode icon is pencil (0xF448)")

      ;; Type input and execute command
      (buffer-handle-key sh #\( :char #\()
      (buffer-handle-key sh #\+ :char #\+)
      (buffer-handle-key sh #\Space :char #\Space)
      (buffer-handle-key sh #\1 :char #\1)
      (buffer-handle-key sh #\0 :char #\0)
      (buffer-handle-key sh #\Space :char #\Space)
      (buffer-handle-key sh #\2 :char #\2)
      (buffer-handle-key sh #\0 :char #\0)
      (buffer-handle-key sh #\) :char #\))
      (buffer-handle-key sh :enter)

      (let ((lines (buffer-content-lines sh)))
        (assert-test (> (length lines) 1) "shell buffer has output lines after evaluation")
        (assert-test (some (lambda (l) (search "30" l)) lines) "output history contains 30"))

      ;; Press Escape -> transitions to :normal mode
      (buffer-handle-key sh :escape)
      (assert-test (eq (editor-buffer-helix-mode sh) :normal) "Escape transitions shell-buffer to :normal mode")
      (assert-test (string= (buffer-mode-name sh) "NOR") "mode name is NOR")
      (assert-test (eq (buffer-mode-icon sh) (code-char #xF09A8)) "mode icon is document (0xF09A8)")

      ;; Navigate into scrollback with 'k'
      (let ((prompt-row (editor-buffer-cursor-row sh)))
        (buffer-handle-key sh #\k :char #\k)
        (assert-test (< (editor-buffer-cursor-row sh) prompt-row) "'k' moved cursor up into scrollback history")

        ;; Select line in scrollback with 'x'
        (buffer-handle-key sh #\x :char #\x)
        (assert-test (eq (editor-buffer-helix-mode sh) :normal) "'x' in scrollback keeps :normal mode")
        (assert-test (numberp (editor-buffer-selection-anchor-row sh)) "'x' activated line selection in scrollback")

        ;; Yank line with 'y'
        (buffer-handle-key sh #\y :char #\y)
        (assert-test (> (length (editor-buffer-clipboard sh)) 0) "'y' yanked scrollback line into clipboard")

        ;; Scrollback lines are read-only: 'd' will not delete history
        (let ((before-count (length (editor-buffer-lines sh))))
          (buffer-handle-key sh #\d :char #\d)
          (assert-test (= (length (editor-buffer-lines sh)) before-count) "'d' on scrollback is blocked (read-only)")
          (assert-test (search "read-only" (editor-buffer-status-message sh)) "status message warns scrollback is read-only"))

        ;; Paste with 'p' pastes clipboard into active prompt line!
        (buffer-handle-key sh #\p :char #\p)
        (let* ((lines (buffer-content-lines sh))
               (last-l (first (last lines))))
          (assert-test (search (string-trim '(#\Newline #\Return) (editor-buffer-clipboard sh)) last-l)
                       "'p' pasted clipboard text into shell prompt"))

        ;; Press 'i' jumps cursor to prompt line in :insert mode
        (buffer-handle-key sh #\i :char #\i)
        (assert-test (eq (editor-buffer-helix-mode sh) :insert) "'i' transitioned back to :insert mode")
        (assert-test (= (editor-buffer-cursor-row sh) (1- (length (buffer-content-lines sh))))
                     "cursor jumped back to prompt line")))))

;;; ---------------------------------------------------------------------------
;;; 14. Minimal Modeline & Nerd Font Icons
;;; ---------------------------------------------------------------------------

(defun test-minimal-modeline-and-icons ()
  (def-test-group "14. Minimal Modeline & Nerd Font Icons"
    (let ((ed (make-instance 'editor-buffer :name "test.lisp")))
      ;; Normal mode: document icon 0xF09A8
      (assert-test (eq (buffer-mode-icon ed) (code-char #xF09A8)) "normal mode icon is document (0xF09A8)")
      ;; Insert mode: pencil icon 0xF448
      (setf (editor-buffer-helix-mode ed) :insert)
      (assert-test (eq (buffer-mode-icon ed) (code-char #xF448)) "insert mode icon is pencil (0xF448)")
      ;; Select mode: I-beam icon 0xF246
      (setf (editor-buffer-helix-mode ed) :select)
      (assert-test (eq (buffer-mode-icon ed) (code-char #xF246)) "select mode icon is I-beam (0xF246)"))))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun run-all-tests ()
  (setf *test-passes* 0
        *test-failures* 0)
  (format t "~%================================================================~%")
  (format t " >>> LispOS Polymorphic CLOS Buffer & Helix Editor Test Suite <<<~%")
  (format t "================================================================~%")

  (test-buffer-registry)
  (test-shell-buffer-protocol)
  (test-helix-insert-mode)
  (test-helix-normal-motions)
  (test-helix-command-mode-and-persistence)
  (test-helix-selection-mode)
  (test-helix-undo-redo)
  (test-helix-space-leader)
  (test-scratch-buffer-flow)
  (test-shell-commands)
  (test-helix-advanced-features)
  (test-word-motions-and-del-discrepancies)
  (test-shell-buffer-helix-scrollback)
  (test-minimal-modeline-and-icons)

  (format t "~%================================================================~%")
  (format t " Test Results Summary: ~D Passed, ~D Failed.~%" *test-passes* *test-failures*)
  (format t "================================================================~%~%")
  (if (zerop *test-failures*)
      (progn
        (format t " SUCCESS: All LispOS buffer & editor tests passed successfully!~%~%")
        (uiop:quit 0))
      (progn
        (format t " FAILURE: ~D tests failed!~%~%" *test-failures*)
        (uiop:quit 1))))

(run-all-tests)
