;;;; src/paths.lisp - Central Pathname Constants and Logical Pathnames for LispOS

(in-package :lispos.paths)

(defparameter +root+ "/")

(defparameter +system-root+ "/System/")
(defparameter +system-core+ "/System/Core/")
(defparameter +system-libraries+ "/System/Libraries/")
(defparameter +system-source+ "/System/Source/")
(defparameter +system-configuration+ "/System/Configuration/")
(defparameter +system-runtime+ "/System/Runtime/")

;; Kernel pseudo-filesystems (hidden away from /)
(defparameter +proc-root+ "/System/Kernel/Processes/")
(defparameter +sys-root+ "/System/Kernel/Hardware/")
(defparameter +dev-root+ "/System/Kernel/Devices/")

;; Device paths
(defparameter +drm-card0+ "/System/Kernel/Devices/dri/card0")
(defparameter +serial-console+ "/System/Kernel/Devices/ttyS0")

;; Userland folders
(defparameter +applications-root+ "/Applications/")
(defparameter +library-root+ "/Library/")
(defparameter +library-fonts+ "/Library/Fonts/")
(defparameter +users-root+ "/Users/")
(defparameter +admin-home+ "/Users/Admin/")

(defun setup-logical-pathnames ()
  "Map SBCL's SYS logical pathname and LISPOS to the non-FHS directory hierarchy."
  (setf (logical-pathname-translations "SYS")
        '(("SRC;**;*.*.*" "/System/Source/SBCL/src/**/*.*")
          ("CONTRIB;**;*.*.*" "/System/Source/SBCL/contrib/**/*.*")))
  (setf (logical-pathname-translations "LISPOS")
        '(("**;*.*.*" "/System/Source/LispOS/**/*.*"))))

(defun ensure-system-hierarchy ()
  "Ensure all directories in the LispOS hierarchy exist on the filesystem."
  (dolist (dir (list +system-root+
                     +system-core+
                     +system-libraries+
                     +system-source+
                     +system-configuration+
                     +system-runtime+
                     "/System/Kernel/"
                     +proc-root+
                     +sys-root+
                     +dev-root+
                     +applications-root+
                     +library-root+
                     +library-fonts+
                     +users-root+
                     +admin-home+))
    (ignore-errors
     (let ((path-str (namestring dir)))
       (ensure-directories-exist
        (if (and (> (length path-str) 0) (char= (char path-str (1- (length path-str))) #\/))
            path-str
            (concatenate 'string path-str "/")))))))
