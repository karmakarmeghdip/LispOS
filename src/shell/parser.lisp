;;;; src/shell/parser.lisp - Shell Line Tokenizer and S-Expression Syntax Parser

(in-package :lispos.shell)

(declaim (ftype (function ((or string null)) pathname) resolve-path))
(defun resolve-path (path-str)
  "Resolve PATH-STR relative to *default-pathname-defaults*, expanding ~ to Admin home."
  (let* ((clean-str (string-trim '(#\Space #\Tab #\Newline #\Return) (or path-str "")))
         (admin-home (ignore-errors (namestring (pathname lispos.paths:+admin-home+))))
         (home-dir (if (and admin-home (> (length admin-home) 0))
                       (if (char= (char admin-home (1- (length admin-home))) #\/)
                           admin-home
                           (concatenate 'string admin-home "/"))
                       "/Users/Admin/")))
    (cond
      ((or (zerop (length clean-str)) (string= clean-str "~") (string= clean-str "~/"))
       (pathname home-dir))
      ((and (>= (length clean-str) 2) (string= (subseq clean-str 0 2) "~/"))
       (pathname (concatenate 'string home-dir (subseq clean-str 2))))
      ((char= (char clean-str 0) #\/)
       (pathname clean-str))
      (t
       ;; Ensure *default-pathname-defaults* is treated as a directory pathname with trailing slash
       (let* ((cwd-str (namestring *default-pathname-defaults*))
              (ensured-cwd (if (and (> (length cwd-str) 0)
                                    (char= (char cwd-str (1- (length cwd-str))) #\/))
                               *default-pathname-defaults*
                               (pathname (concatenate 'string cwd-str "/")))))
         (merge-pathnames clean-str ensured-cwd))))))

(declaim (ftype (function (string) list) tokenize-shell-line))
(defun tokenize-shell-line (line)
  "Parse command line into token strings, respecting single and double quotes."
  (declare (type string line)
           (optimize (speed 3) (safety 1)))
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

(declaim (ftype (function (string) boolean) balanced-parentheses-p))
(defun balanced-parentheses-p (text)
  "Verify whether parentheses, strings, comments, and character literals in TEXT
are fully balanced according to Common Lisp reader syntax.
Returns T if fully balanced, NIL otherwise."
  (declare (type string text)
           (optimize (speed 3) (safety 1)))
  (let ((len (length text))
        (depth 0)
        (i 0)
        (in-string nil)
        (in-pipe nil)
        (block-comment-depth 0))
    (declare (type fixnum len depth i block-comment-depth)
             (type boolean in-string in-pipe))
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

(declaim (ftype (function (string) list) split-lines))
(defun split-lines (str)
  "Split string STR into a list of lines without trailing newlines."
  (declare (type string str)
           (optimize (speed 3) (safety 1)))
  (let ((lines '())
        (start 0)
        (len (length str)))
    (declare (type fixnum start len))
    (loop for i of-type fixnum from 0 below len do
      (when (char= (char str i) #\Newline)
        (push (subseq str start i) lines)
        (setf start (1+ i))))
    (when (< start len)
      (push (subseq str start len) lines))
    (nreverse lines)))
