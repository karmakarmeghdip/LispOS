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
      (assert-test (string= (buffer-mode-name sh) "SHELL") "buffer-mode-name is SHELL")
      (assert-test (eq (buffer-mode sh) :shell) "buffer-mode is :shell")
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

      (buffer-handle-key ed #\w :char #\w)
      (assert-test (= (editor-buffer-cursor-col ed) 10) "'w' jumps to 'brown' (col 10)")

      (buffer-handle-key ed #\b :char #\b)
      (assert-test (= (editor-buffer-cursor-col ed) 4) "'b' jumps back to 'quick' (col 4)")

      (buffer-handle-key ed #\$ :char #\$)
      (assert-test (= (editor-buffer-cursor-col ed) 18) "'$' jumps to end of line")

      (buffer-handle-key ed #\0 :char #\0)
      (assert-test (= (editor-buffer-cursor-col ed) 0) "'0' jumps to col 0")

      (buffer-handle-key ed #\j :char #\j)
      (assert-test (= (editor-buffer-cursor-row ed) 1) "'j' moves down to row 1")

      (buffer-handle-key ed #\k :char #\k)
      (assert-test (= (editor-buffer-cursor-row ed) 0) "'k' moves up to row 0")

      (buffer-handle-key ed #\x :char #\x)
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "he quick brown fox") "'x' deletes char under cursor")

      (buffer-handle-key ed #\d :char #\d)
      (buffer-handle-key ed #\d :char #\d)
      (assert-test (= (length (editor-buffer-lines ed)) 1) "'dd' deleted current line")
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "jumps over the dog") "remaining line is second line")
      (assert-test (string= (editor-buffer-clipboard ed) "he quick brown fox") "'dd' saved deleted line to clipboard")

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
;;; 6. Helix Selection Mode (v, V, motions, y, d, c)
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
      (assert-test (string= (editor-buffer-clipboard ed) "First ") "yanked exact selected text into clipboard")

      ;; Test 'V' line selection
      (buffer-handle-key ed #\V :char #\V)
      (assert-test (eq (editor-buffer-helix-mode ed) :select) "'V' enters select mode for line")
      (assert-test (= (editor-buffer-selection-anchor-col ed) 0) "anchor col is 0 for line select")

      ;; Delete selection with 'd'
      (buffer-handle-key ed #\d :char #\d)
      (assert-test (eq (editor-buffer-helix-mode ed) :normal) "'d' on selection returns to :normal")
      (assert-test (= (length (editor-buffer-lines ed)) 1) "entire first line deleted")
      (assert-test (string= (aref (editor-buffer-lines ed) 0) "Second line text") "remaining line is second line"))))

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

      ;; Delete 'I' with 'x'
      (buffer-handle-key ed #\x :char #\x)
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
