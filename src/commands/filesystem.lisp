;;;; src/commands/filesystem.lisp - Built-in Filesystem Commands for LispOS Shell
;;;; Implements cd, pwd, ls, cat, touch, mkdir, rm.

(in-package :lispos.shell)

;;; ---------------------------------------------------------------------------
;;; pwd - Print Current Working Directory
;;; ---------------------------------------------------------------------------

(define-shell-command pwd (engine &rest args)
  "Print the current working directory pathname."
  (declare (ignore engine args))
  (format t "~A~%" (get-cwd-namestring))
  nil)

;;; ---------------------------------------------------------------------------
;;; cd - Change Working Directory
;;; ---------------------------------------------------------------------------

(define-shell-command cd (engine &optional (target "~") &rest rest)
  "Change current working directory and update LispOS pathname defaults."
  (declare (ignore engine rest))
  (let* ((resolved (resolve-path target))
         (probe (probe-file resolved)))
    (if probe
        (let* ((is-dir (null (pathname-name probe)))
               (dir-path (if is-dir
                             probe
                             (pathname (directory-namestring probe)))))
          (setf *default-pathname-defaults* (truename dir-path))
          (ignore-errors (sb-posix:chdir (namestring *default-pathname-defaults*)))
          nil)
        (progn
          (format *error-output* "cd: no such file or directory: ~A~%" target)
          nil))))

;;; ---------------------------------------------------------------------------
;;; ls - List Directory Contents
;;; ---------------------------------------------------------------------------

(define-shell-command ls (engine &rest args)
  "List directory contents with optional -l (long format) and -a (all files)."
  (declare (ignore engine))
  (let ((long-format nil)
        (all-files nil)
        (targets '()))
    ;; Parse flags and positional targets
    (dolist (arg args)
      (if (and (> (length arg) 1) (char= (char arg 0) #\-))
          (loop for i from 1 below (length arg) do
            (case (char arg i)
              (#\l (setf long-format t))
              (#\a (setf all-files t))))
          (push arg targets)))

    (let* ((target-dir (if targets (first (nreverse targets)) "."))
           (resolved (resolve-path target-dir))
           (truename (probe-file resolved)))
      (if (null truename)
          (format *error-output* "ls: cannot access '~A': No such file or directory~%" target-dir)
          (let* ((base-str (namestring truename))
                 (dir-str (if (and (> (length base-str) 0)
                                   (char= (char base-str (1- (length base-str))) #\/))
                              base-str
                              (concatenate 'string base-str "/")))
                 (entries (ignore-errors (directory (pathname (concatenate 'string dir-str "*.*"))))))
            ;; Sort entries alphabetically
            (setf entries
                  (sort (copy-list entries) #'string<
                        :key (lambda (p)
                               (let ((name (file-namestring p)))
                                 (if (and name (> (length name) 0))
                                     name
                                     (or (car (last (pathname-directory p))) ""))))))
            (dolist (p entries)
              (let* ((is-dir (null (pathname-name p)))
                     (name (if is-dir
                               (format nil "~A/" (car (last (pathname-directory p))))
                               (file-namestring p)))
                     (is-hidden (and (> (length name) 0) (char= (char name 0) #\.))))
                (when (or all-files (not is-hidden))
                  (if long-format
                      (let ((size (or (ignore-errors
                                       (let ((st (sb-posix:stat (namestring p))))
                                         (sb-posix:stat-size st)))
                                      0))
                            (type-char (if is-dir "d" "-")))
                        (format t "~Arw-r--r-- ~8D  ~A~%" type-char size name))
                      (format t "~A~%" name)))))))))
  nil)

;;; ---------------------------------------------------------------------------
;;; cat - Display File Contents
;;; ---------------------------------------------------------------------------

(define-shell-command cat (engine &rest files)
  "Display contents of specified files to stdout."
  (declare (ignore engine))
  (if (null files)
      nil
      (dolist (f files)
        (let* ((resolved (resolve-path f))
               (probe (probe-file resolved)))
          (cond
            ((null probe)
             (format *error-output* "cat: ~A: No such file~%" f))
            ((null (pathname-name probe))
             (format *error-output* "cat: ~A: Is a directory~%" f))
            (t
             (handler-case
                 (with-open-file (in probe :direction :input)
                   (loop for line = (read-line in nil nil)
                         while line do (write-line line)))
               (error (c)
                 (format *error-output* "cat: error reading '~A': ~A~%" f c))))))))
  nil)

;;; ---------------------------------------------------------------------------
;;; touch - Create Empty File or Update Timestamp
;;; ---------------------------------------------------------------------------

(define-shell-command touch (engine &rest files)
  "Create an empty file if it doesn't exist, or update write date."
  (declare (ignore engine))
  (dolist (f files)
    (let ((resolved (resolve-path f)))
      (handler-case
          (with-open-file (out resolved :direction :output
                                        :if-exists :append
                                        :if-does-not-exist :create)
            (declare (ignore out)))
        (error (c)
          (format *error-output* "touch: cannot touch '~A': ~A~%" f c)))))
  nil)

;;; ---------------------------------------------------------------------------
;;; mkdir - Create Directories
;;; ---------------------------------------------------------------------------

(define-shell-command mkdir (engine &rest args)
  "Create directories specified in ARGS."
  (declare (ignore engine))
  (let ((dirs (remove-if (lambda (a) (and (> (length a) 0) (char= (char a 0) #\-))) args)))
    (dolist (d dirs)
      (let* ((resolved (resolve-path d))
             (dir-str (namestring resolved))
             (ensured-str (if (char= (char dir-str (1- (length dir-str))) #\/)
                              dir-str
                              (concatenate 'string dir-str "/"))))
        (handler-case
            (ensure-directories-exist ensured-str)
          (error (c)
            (format *error-output* "mkdir: cannot create directory '~A': ~A~%" d c))))))
  nil)

;;; ---------------------------------------------------------------------------
;;; rm - Remove Files or Directories
;;; ---------------------------------------------------------------------------

(define-shell-command rm (engine &rest args)
  "Remove files or directories (-r for recursive directory removal)."
  (declare (ignore engine))
  (let ((recursive nil)
        (targets '()))
    (dolist (a args)
      (if (and (> (length a) 1) (char= (char a 0) #\-))
          (when (or (search "r" a) (search "R" a))
            (setf recursive t))
          (push a targets)))
    (dolist (targ (nreverse targets))
      (let* ((resolved (resolve-path targ))
             (probe (probe-file resolved)))
        (cond
          ((null probe)
           (format *error-output* "rm: cannot remove '~A': No such file or directory~%" targ))
          ((null (pathname-name probe))
           ;; Directory
           (if recursive
               (handler-case
                   (sb-posix:rmdir (namestring probe))
                 (error (c)
                   (format *error-output* "rm: cannot remove directory '~A': ~A~%" targ c)))
               (format *error-output* "rm: cannot remove '~A': Is a directory~%" targ)))
          (t
           (handler-case
               (delete-file probe)
             (error (c)
               (format *error-output* "rm: cannot delete '~A': ~A~%" targ c))))))))
  nil)
