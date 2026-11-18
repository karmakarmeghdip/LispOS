;;;; src/hal/drm/ioctl.lisp - Direct DRM/KMS Dumb-Buffer Graphics for LispOS

(in-package :lispos.drm)

(pushnew #P"/System/Libraries/" cffi:*foreign-library-directories* :test #'equal)
(pushnew #P"/usr/lib/" cffi:*foreign-library-directories* :test #'equal)

(cffi:define-foreign-library libdrm
  (:unix (:or "/System/Libraries/libdrm.so.2" "libdrm.so.2" "libdrm.so"))
  (t (:default "libdrm")))

(cffi:use-foreign-library libdrm)

;; IOCTL Constants
(defconstant +drm-ioctl-mode-create-dumb+  #xC02064B2)
(defconstant +drm-ioctl-mode-map-dumb+     #xC01064B3)
(defconstant +drm-ioctl-mode-destroy-dumb+ #xC00464B4)

(defconstant +o-cloexec+ #x80000)

;; DRM Connection status
(defconstant +drm-mode-connected+ 1)

;; libdrm CFFI function bindings
(cffi:defcfun ("drmSetMaster" %drm-set-master) :int
  (fd :int))

(cffi:defcfun ("drmDropMaster" %drm-drop-master) :int
  (fd :int))

(cffi:defcfun ("drmModeGetResources" %drm-mode-get-resources) :pointer
  (fd :int))

(cffi:defcfun ("drmModeFreeResources" %drm-mode-free-resources) :void
  (res :pointer))

(cffi:defcfun ("drmModeGetConnector" %drm-mode-get-connector) :pointer
  (fd :int)
  (connector-id :uint32))

(cffi:defcfun ("drmModeFreeConnector" %drm-mode-free-connector) :void
  (connector :pointer))

(cffi:defcfun ("drmModeAddFB" %drm-mode-add-fb) :int
  (fd :int)
  (width :uint32)
  (height :uint32)
  (depth :uint8)
  (bpp :uint8)
  (pitch :uint32)
  (bo-handle :uint32)
  (buf-id-ptr :pointer))

(cffi:defcfun ("drmModeRmFB" %drm-mode-rm-fb) :int
  (fd :int)
  (buffer-id :uint32))

(cffi:defcfun ("drmModeSetCrtc" %drm-mode-set-crtc) :int
  (fd :int)
  (crtc-id :uint32)
  (buffer-id :uint32)
  (x :uint32)
  (y :uint32)
  (connectors-ptr :pointer)
  (count :int)
  (mode-ptr :pointer))

(cffi:defcfun ("drmModeDirtyFB" %drm-mode-dirty-fb) :int
  (fd :int)
  (buffer-id :uint32)
  (clips :pointer)
  (num-clips :uint32))

(defstruct (drm-state
            (:constructor %make-drm-state))
  (fd -1 :type fixnum)
  (crtc-id 0 :type (unsigned-byte 32))
  (connector-id 0 :type (unsigned-byte 32))
  (fb-id 0 :type (unsigned-byte 32))
  (handle 0 :type (unsigned-byte 32))
  (width 0 :type (unsigned-byte 16))
  (height 0 :type (unsigned-byte 16))
  (pitch 0 :type (unsigned-byte 32))
  (size 0 :type (unsigned-byte 64))
  (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer)
  (mmap-addr 0 :type (unsigned-byte 64)))

(defstruct (drm-mode-info
            (:constructor %make-drm-mode-info))
  (index 0 :type fixnum)
  (name "" :type string)
  (width 0 :type (unsigned-byte 16))
  (height 0 :type (unsigned-byte 16))
  (vrefresh 0 :type (unsigned-byte 32))
  (flags 0 :type (unsigned-byte 32))
  (type 0 :type (unsigned-byte 32))
  (preferred-p nil :type boolean)
  (current-p nil :type boolean))

(defvar *mode-changed-hooks* nil
  "List of functions (lambda (width height pitch)) invoked when display mode changes.")

(defun parse-resolution-string (str)
  "Parse '1920x1080' or '1920 1080' into (values width height)."
  (when (stringp str)
    (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) str))
           (sep-pos (or (position #\x trimmed :test #'char-equal)
                        (position #\Space trimmed)
                        (position #\, trimmed))))
      (when sep-pos
        (let ((w (parse-integer (subseq trimmed 0 sep-pos) :junk-allowed t))
              (h (parse-integer (subseq trimmed (1+ sep-pos)) :junk-allowed t)))
          (when (and w h (> w 0) (> h 0))
            (values w h)))))))

(defun detect-boot-resolution ()
  "Detect requested resolution from /System/Configuration/display.sexp or kernel command line."
  ;; 1. Check declarative config /System/Configuration/display.sexp
  (let ((config-file (merge-pathnames "display.sexp" lispos.paths:+system-configuration+)))
    (when (probe-file config-file)
      (ignore-errors
       (with-open-file (in config-file :direction :input)
         (let ((cfg (read in nil nil)))
           (when (listp cfg)
             (let ((res (getf cfg :resolution)))
               (cond
                 ((and (listp res) (= (length res) 2))
                  (return-from detect-boot-resolution (values (first res) (second res))))
                 ((stringp res)
                  (multiple-value-bind (w h) (parse-resolution-string res)
                    (when (and w h)
                      (return-from detect-boot-resolution (values w h)))))))))))))
  ;; 2. Check kernel command line (/System/Kernel/Processes/cmdline or /proc/cmdline)
  (let* ((cmdline-path (merge-pathnames "cmdline" lispos.paths:+proc-root+))
         (path (or (probe-file cmdline-path)
                   (probe-file "/System/Kernel/Processes/cmdline")
                   (probe-file "/proc/cmdline"))))
    (when path
      (let ((cmdline (ignore-errors
                      (with-open-file (in path :direction :input)
                        (read-line in nil "")))))
        (when (and cmdline (> (length cmdline) 0))
          (let ((tokens '())
                (start 0)
                (len (length cmdline)))
            ;; Simple whitespace split
            (loop while (< start len) do
              (let ((pos (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                      cmdline :start start)))
                (if pos
                    (progn
                      (when (> pos start)
                        (push (subseq cmdline start pos) tokens))
                      (setf start (1+ pos)))
                    (progn
                      (push (subseq cmdline start len) tokens)
                      (setf start len)))))
            (dolist (token (nreverse tokens))
              (cond
                ((and (>= (length token) 15)
                      (string-equal (subseq token 0 15) "lispos.display="))
                 (multiple-value-bind (w h) (parse-resolution-string (subseq token 15))
                   (when (and w h)
                     (return-from detect-boot-resolution (values w h)))))
                ((and (>= (length token) 6)
                      (string-equal (subseq token 0 6) "video="))
                 (let* ((val (subseq token 6))
                        (colon-pos (position #\: val)))
                   (multiple-value-bind (w h) (parse-resolution-string (if colon-pos (subseq val (1+ colon-pos)) val))
                     (when (and w h)
                       (return-from detect-boot-resolution (values w h))))))
                ((and (>= (length token) 8)
                      (string-equal (subseq token 0 8) "display="))
                 (multiple-value-bind (w h) (parse-resolution-string (subseq token 8))
                   (when (and w h)
                     (return-from detect-boot-resolution (values w h))))))))))))
  (values nil nil))

(defun make-drm-state (&key fd crtc-id connector-id fb-id handle width height pitch size sap mmap-addr)
  (%make-drm-state :fd (or fd -1)
                   :crtc-id (or crtc-id 0)
                   :connector-id (or connector-id 0)
                   :fb-id (or fb-id 0)
                   :handle (or handle 0)
                   :width (or width 0)
                   :height (or height 0)
                   :pitch (or pitch 0)
                   :size (or size 0)
                   :sap (or sap (sb-sys:int-sap 0))
                   :mmap-addr (or mmap-addr 0)))

(defvar *drm-state* nil
  "Global active DRM state.")

(declaim (type (or null drm-state) *drm-state*))

(defun find-connected-connector (fd res-ptr)
  "Iterate over connectors in resources and return (connector-ptr connector-id)."
  (let ((count-connectors (cffi:mem-ref (cffi:inc-pointer res-ptr 32) :int))
        (connectors-ptr (cffi:mem-ref (cffi:inc-pointer res-ptr 40) :pointer)))
    (dotimes (i count-connectors nil)
      (let* ((conn-id (cffi:mem-aref connectors-ptr :uint32 i))
             (conn (%drm-mode-get-connector fd conn-id)))
        (unless (cffi:null-pointer-p conn)
          (let ((connection (cffi:mem-ref (cffi:inc-pointer conn 16) :int))
                (count-modes (cffi:mem-ref (cffi:inc-pointer conn 32) :int)))
            (if (and (= connection +drm-mode-connected+) (> count-modes 0))
                (return-from find-connected-connector (values conn conn-id))
                (%drm-mode-free-connector conn))))))))

(defun init-drm (&optional (device-or-width lispos.paths:+drm-card0+) (height-or-nil nil))
  "Initialize KMS framebuffer on device-path.
Supports:
  (init-drm)                       -> Auto-detect native resolution from EDID / kernel cmdline
  (init-drm 1920 1080)             -> Explicit width & height
  (init-drm \"/dev/dri/card0\")     -> Explicit device path with auto resolution"
  (let ((device-path lispos.paths:+drm-card0+)
        (target-width nil)
        (target-height nil))
    (cond
      ((and (integerp device-or-width) (integerp height-or-nil))
       (setf target-width device-or-width
             target-height height-or-nil))
      ((or (stringp device-or-width) (pathnamep device-or-width))
       (setf device-path (namestring device-or-width))))

    (let ((path (if (probe-file device-path)
                    device-path
                    (if (probe-file "/dev/dri/card0")
                        "/dev/dri/card0"
                        device-path))))
      (format t "[DRM] Opening DRM device ~A~%" path)
      (let ((fd (sb-posix:open path (logior sb-posix:o-rdwr +o-cloexec+))))
        (when (< fd 0)
          (error "[DRM] Failed to open ~A: errno ~A" path (sb-alien:get-errno)))
        (%drm-set-master fd)
        (let ((res (%drm-mode-get-resources fd)))
          (when (cffi:null-pointer-p res)
            (sb-posix:close fd)
            (error "[DRM] drmModeGetResources failed"))
          (unwind-protect
               (multiple-value-bind (conn conn-id)
                   (find-connected-connector fd res)
                 (unless conn
                   (error "[DRM] No connected display connector found"))
                 (unwind-protect
                      (let* ((count-crtcs (cffi:mem-ref (cffi:inc-pointer res 16) :int))
                             (crtcs-ptr (cffi:mem-ref (cffi:inc-pointer res 24) :pointer))
                             (crtc-id (if (> count-crtcs 0)
                                          (cffi:mem-aref crtcs-ptr :uint32 0)
                                          (error "[DRM] No CRTC found")))
                             (count-modes (cffi:mem-ref (cffi:inc-pointer conn 32) :int))
                             (modes-ptr (cffi:mem-ref (cffi:inc-pointer conn 40) :pointer))
                             (req-w (or target-width (nth-value 0 (detect-boot-resolution))))
                             (req-h (or target-height (nth-value 1 (detect-boot-resolution))))
                             (chosen-mode-ptr nil))
                        ;; 1. Check explicit requested resolution match
                        (when (and req-w req-h)
                          (dotimes (i count-modes)
                            (let* ((mptr (cffi:inc-pointer modes-ptr (* i 68)))
                                   (w (cffi:mem-ref (cffi:inc-pointer mptr 4) :uint16))
                                   (h (cffi:mem-ref (cffi:inc-pointer mptr 14) :uint16)))
                              (when (and (= w req-w) (= h req-h))
                                (setf chosen-mode-ptr mptr)
                                (return)))))
                        ;; 2. If not requested or not found, look for PREFERRED native mode
                        (unless chosen-mode-ptr
                          (dotimes (i count-modes)
                            (let* ((mptr (cffi:inc-pointer modes-ptr (* i 68)))
                                   (type (cffi:mem-ref (cffi:inc-pointer mptr 32) :uint32)))
                              (when (logtest type 8) ; DRM_MODE_TYPE_PREFERRED
                                (setf chosen-mode-ptr mptr)
                                (return)))))
                        ;; 3. Fall back to mode index 0
                        (unless chosen-mode-ptr
                          (setf chosen-mode-ptr modes-ptr))

                        (let ((width (cffi:mem-ref (cffi:inc-pointer chosen-mode-ptr 4) :uint16))
                              (height (cffi:mem-ref (cffi:inc-pointer chosen-mode-ptr 14) :uint16)))
                          (format t "[DRM] Selected Mode: ~Ax~A on connector ~A, CRTC ~A~%"
                                  width height conn-id crtc-id)
                          ;; Allocate dumb buffer
                          (cffi:with-foreign-pointer (create-req 32)
                            (dotimes (i 32) (setf (cffi:mem-aref create-req :uint8 i) 0))
                            (setf (cffi:mem-ref (cffi:inc-pointer create-req 0) :uint32) height)
                            (setf (cffi:mem-ref (cffi:inc-pointer create-req 4) :uint32) width)
                            (setf (cffi:mem-ref (cffi:inc-pointer create-req 8) :uint32) 32) ;; 32 bpp
                            (let ((ret (lispos.sys:ioctl fd +drm-ioctl-mode-create-dumb+ create-req)))
                              (when (< ret 0)
                                (error "[DRM] CREATE_DUMB ioctl failed"))
                              (let ((handle (cffi:mem-ref (cffi:inc-pointer create-req 16) :uint32))
                                    (pitch  (cffi:mem-ref (cffi:inc-pointer create-req 20) :uint32))
                                    (size   (cffi:mem-ref (cffi:inc-pointer create-req 24) :uint64)))
                                (format t "[DRM] Dumb buffer created: handle ~A, pitch ~A, size ~A bytes~%"
                                        handle pitch size)
                                ;; Add Framebuffer
                                (cffi:with-foreign-pointer (fb-id-ptr 4)
                                  (let ((fb-ret (%drm-mode-add-fb fd width height 24 32 pitch handle fb-id-ptr)))
                                    (when (< fb-ret 0)
                                      (error "[DRM] drmModeAddFB failed"))
                                    (let ((fb-id (cffi:mem-ref fb-id-ptr :uint32)))
                                      ;; Map dumb buffer
                                      (cffi:with-foreign-pointer (map-req 16)
                                        (dotimes (i 16) (setf (cffi:mem-aref map-req :uint8 i) 0))
                                        (setf (cffi:mem-ref (cffi:inc-pointer map-req 0) :uint32) handle)
                                        (let ((map-ret (lispos.sys:ioctl fd +drm-ioctl-mode-map-dumb+ map-req)))
                                          (when (< map-ret 0)
                                            (error "[DRM] MAP_DUMB ioctl failed"))
                                          (let* ((offset (cffi:mem-ref (cffi:inc-pointer map-req 8) :uint64))
                                                 (mmap-res (sb-posix:mmap (sb-sys:int-sap 0)
                                                                          size
                                                                          (logior sb-posix:prot-read sb-posix:prot-write)
                                                                          sb-posix:map-shared
                                                                          fd
                                                                          offset)))
                                            (format t "[DRM] Framebuffer mmapped at SAP ~A~%" mmap-res)
                                            ;; Set CRTC
                                            (cffi:with-foreign-pointer (conns-arr 4)
                                              (setf (cffi:mem-ref conns-arr :uint32) conn-id)
                                              (let ((crtc-ret (%drm-mode-set-crtc fd crtc-id fb-id 0 0 conns-arr 1 chosen-mode-ptr)))
                                                (when (< crtc-ret 0)
                                                  (error "[DRM] drmModeSetCrtc failed"))))
                                            (setf *drm-state*
                                                  (make-drm-state :fd fd
                                                                  :crtc-id crtc-id
                                                                  :connector-id conn-id
                                                                  :fb-id fb-id
                                                                  :handle handle
                                                                  :width width
                                                                  :height height
                                                                  :pitch pitch
                                                                  :size size
                                                                  :sap mmap-res
                                                                  :mmap-addr (sb-sys:sap-int mmap-res)))
                                            *drm-state*))))))))))
                     (%drm-mode-free-connector conn)))
            (%drm-mode-free-resources res))))))))

(defun list-modes (&optional (drm-state *drm-state*))
  "Query active DRM connector and return list of all supported DRM-MODE-INFO structs."
  (let ((fd (if drm-state (drm-state-fd drm-state) -1))
        (conn-id (if drm-state (drm-state-connector-id drm-state) 0))
        (cur-w (if drm-state (drm-state-width drm-state) 0))
        (cur-h (if drm-state (drm-state-height drm-state) 0))
        (close-needed nil))
    (when (< fd 0)
      ;; Temporarily open DRM device to query modes
      (let ((path (if (probe-file lispos.paths:+drm-card0+)
                      lispos.paths:+drm-card0+
                      (if (probe-file "/dev/dri/card0") "/dev/dri/card0" nil))))
        (unless path (return-from list-modes nil))
        (setf fd (ignore-errors (sb-posix:open path (logior sb-posix:o-rdwr +o-cloexec+))))
        (when (or (null fd) (< fd 0)) (return-from list-modes nil))
        (setf close-needed t)))
    (unwind-protect
         (let ((res (%drm-mode-get-resources fd)))
           (unless (cffi:null-pointer-p res)
             (unwind-protect
                  (multiple-value-bind (conn actual-conn-id)
                      (if (> conn-id 0)
                          (values (%drm-mode-get-connector fd conn-id) conn-id)
                          (find-connected-connector fd res))
                    (declare (ignore actual-conn-id))
                    (unless (cffi:null-pointer-p conn)
                      (unwind-protect
                           (let* ((count-modes (cffi:mem-ref (cffi:inc-pointer conn 32) :int))
                                  (modes-ptr (cffi:mem-ref (cffi:inc-pointer conn 40) :pointer))
                                  (result nil))
                             (dotimes (i count-modes (nreverse result))
                               (let* ((mptr (cffi:inc-pointer modes-ptr (* i 68)))
                                      (w (cffi:mem-ref (cffi:inc-pointer mptr 4) :uint16))
                                      (h (cffi:mem-ref (cffi:inc-pointer mptr 14) :uint16))
                                      (vrefresh (cffi:mem-ref (cffi:inc-pointer mptr 24) :uint32))
                                      (flags (cffi:mem-ref (cffi:inc-pointer mptr 28) :uint32))
                                      (type (cffi:mem-ref (cffi:inc-pointer mptr 32) :uint32))
                                      (name (cffi:foreign-string-to-lisp (cffi:inc-pointer mptr 36)))
                                      (pref-p (logtest type 8))
                                      (cur-p (and (= w cur-w) (= h cur-h))))
                                 (push (%make-drm-mode-info :index i
                                                            :name name
                                                            :width w
                                                            :height h
                                                            :vrefresh vrefresh
                                                            :flags flags
                                                            :type type
                                                            :preferred-p pref-p
                                                            :current-p cur-p)
                                       result))))
                        (%drm-mode-free-connector conn))))
               (%drm-mode-free-resources res))))
      (when close-needed
        (ignore-errors (sb-posix:close fd))))))

(defun find-mode (target &optional (modes (list-modes)))
  "Find a mode from MODES matching TARGET (:preferred, '1920x1080', or '(1920 1080))."
  (cond
    ((or (eq target :preferred) (eq target :auto))
     (or (find-if #'drm-mode-info-preferred-p modes)
         (first modes)))
    ((stringp target)
     (multiple-value-bind (w h) (parse-resolution-string target)
       (if (and w h)
           (find-mode (list w h) modes)
           (find target modes :key #'drm-mode-info-name :test #'string-equal))))
    ((and (listp target) (= (length target) 2))
     (let ((tw (first target))
           (th (second target)))
       (find-if (lambda (m)
                  (and (= (drm-mode-info-width m) tw)
                       (= (drm-mode-info-height m) th)))
                modes)))
    (t nil)))

(defun set-mode (width height &optional (drm-state *drm-state*))
  "Dynamically switch KMS display mode on DRM-STATE to WIDTHxHEIGHT.
Allocates a new dumb buffer, re-attaches CRTC, cleans up the old framebuffer,
and notifies *MODE-CHANGED-HOOKS*."
  (let ((state (or drm-state *drm-state*)))
    (unless state
      (error "[DRM] No active DRM state to set mode on"))
    (let* ((fd (drm-state-fd state))
           (conn-id (drm-state-connector-id state))
           (crtc-id (drm-state-crtc-id state))
           (conn (%drm-mode-get-connector fd conn-id)))
      (when (cffi:null-pointer-p conn)
        (error "[DRM] drmModeGetConnector failed for connector ~A" conn-id))
      (unwind-protect
           (let* ((count-modes (cffi:mem-ref (cffi:inc-pointer conn 32) :int))
                  (modes-ptr (cffi:mem-ref (cffi:inc-pointer conn 40) :pointer))
                  (target-mode-ptr nil)
                  (target-w 0)
                  (target-h 0))
             (dotimes (i count-modes)
               (let* ((mptr (cffi:inc-pointer modes-ptr (* i 68)))
                      (w (cffi:mem-ref (cffi:inc-pointer mptr 4) :uint16))
                      (h (cffi:mem-ref (cffi:inc-pointer mptr 14) :uint16)))
                 (when (and (= w width) (= h height))
                   (setf target-mode-ptr mptr
                         target-w w
                         target-h h)
                   (return))))
             (unless target-mode-ptr
               (error "[DRM] Resolution ~Ax~A not supported by display connector ~A"
                      width height conn-id))
             ;; Allocate new dumb buffer
             (cffi:with-foreign-pointer (create-req 32)
               (dotimes (i 32) (setf (cffi:mem-aref create-req :uint8 i) 0))
               (setf (cffi:mem-ref (cffi:inc-pointer create-req 0) :uint32) target-h)
               (setf (cffi:mem-ref (cffi:inc-pointer create-req 4) :uint32) target-w)
               (setf (cffi:mem-ref (cffi:inc-pointer create-req 8) :uint32) 32)
               (let ((ret (lispos.sys:ioctl fd +drm-ioctl-mode-create-dumb+ create-req)))
                 (when (< ret 0)
                   (error "[DRM] CREATE_DUMB ioctl failed: errno ~A" (sb-alien:get-errno)))
                 (let ((handle (cffi:mem-ref (cffi:inc-pointer create-req 16) :uint32))
                       (pitch  (cffi:mem-ref (cffi:inc-pointer create-req 20) :uint32))
                       (size   (cffi:mem-ref (cffi:inc-pointer create-req 24) :uint64)))
                   ;; Register framebuffer
                   (cffi:with-foreign-pointer (fb-id-ptr 4)
                     (let ((fb-ret (%drm-mode-add-fb fd target-w target-h 24 32 pitch handle fb-id-ptr)))
                       (when (< fb-ret 0)
                         (cffi:with-foreign-pointer (dreq 4)
                           (setf (cffi:mem-ref dreq :uint32) handle)
                           (ignore-errors (lispos.sys:ioctl fd +drm-ioctl-mode-destroy-dumb+ dreq)))
                         (error "[DRM] drmModeAddFB failed"))
                       (let ((fb-id (cffi:mem-ref fb-id-ptr :uint32)))
                         ;; Map dumb buffer
                         (cffi:with-foreign-pointer (map-req 16)
                           (dotimes (i 16) (setf (cffi:mem-aref map-req :uint8 i) 0))
                           (setf (cffi:mem-ref (cffi:inc-pointer map-req 0) :uint32) handle)
                           (let ((map-ret (lispos.sys:ioctl fd +drm-ioctl-mode-map-dumb+ map-req)))
                             (when (< map-ret 0)
                               (%drm-mode-rm-fb fd fb-id)
                               (cffi:with-foreign-pointer (dreq 4)
                                 (setf (cffi:mem-ref dreq :uint32) handle)
                                 (ignore-errors (lispos.sys:ioctl fd +drm-ioctl-mode-destroy-dumb+ dreq)))
                               (error "[DRM] MAP_DUMB ioctl failed: errno ~A" (sb-alien:get-errno)))
                             (let* ((offset (cffi:mem-ref (cffi:inc-pointer map-req 8) :uint64))
                                    (mmap-res (sb-posix:mmap (sb-sys:int-sap 0)
                                                             size
                                                             (logior sb-posix:prot-read sb-posix:prot-write)
                                                             sb-posix:map-shared
                                                             fd
                                                             offset)))
                               ;; Set CRTC
                               (cffi:with-foreign-pointer (conns-arr 4)
                                 (setf (cffi:mem-ref conns-arr :uint32) conn-id)
                                 (let ((crtc-ret (%drm-mode-set-crtc fd crtc-id fb-id 0 0 conns-arr 1 target-mode-ptr)))
                                   (when (< crtc-ret 0)
                                     (sb-posix:munmap mmap-res size)
                                     (%drm-mode-rm-fb fd fb-id)
                                     (cffi:with-foreign-pointer (dreq 4)
                                       (setf (cffi:mem-ref dreq :uint32) handle)
                                       (ignore-errors (lispos.sys:ioctl fd +drm-ioctl-mode-destroy-dumb+ dreq)))
                                     (error "[DRM] drmModeSetCrtc failed"))))
                               ;; Clean up old buffer
                               (let ((old-sap (drm-state-sap state))
                                     (old-size (drm-state-size state))
                                     (old-fb-id (drm-state-fb-id state))
                                     (old-handle (drm-state-handle state)))
                                 (ignore-errors (sb-posix:munmap old-sap old-size))
                                 (ignore-errors (%drm-mode-rm-fb fd old-fb-id))
                                 (cffi:with-foreign-pointer (dest-req 4)
                                   (setf (cffi:mem-ref dest-req :uint32) old-handle)
                                   (ignore-errors (lispos.sys:ioctl fd +drm-ioctl-mode-destroy-dumb+ dest-req))))
                               ;; Update state
                               (setf (drm-state-fb-id state) fb-id
                                     (drm-state-handle state) handle
                                     (drm-state-width state) target-w
                                     (drm-state-height state) target-h
                                     (drm-state-pitch state) pitch
                                     (drm-state-size state) size
                                     (drm-state-sap state) mmap-res
                                     (drm-state-mmap-addr state) (sb-sys:sap-int mmap-res))
                               ;; Notify mode change hooks
                               (dolist (hook *mode-changed-hooks*)
                                 (ignore-errors (funcall hook target-w target-h pitch)))
                               state))))))))))
        (%drm-mode-free-connector conn)))))

(defun close-drm ()
  "Release DRM framebuffer and close device."
  (when *drm-state*
    (let ((fd (drm-state-fd *drm-state*))
          (size (drm-state-size *drm-state*))
          (sap (drm-state-sap *drm-state*))
          (fb-id (drm-state-fb-id *drm-state*)))
      (ignore-errors (sb-posix:munmap sap size))
      (ignore-errors (%drm-mode-rm-fb fd fb-id))
      (ignore-errors (%drm-drop-master fd))
      (ignore-errors (sb-posix:close fd)))
    (setf *drm-state* nil)))

(declaim (ftype (function () (or null fixnum)) flush-screen))
(defun flush-screen ()
  "Notify DRM KMS driver that framebuffer contents are dirty and should be transferred to display plane."
  (when *drm-state*
    (let ((fd (drm-state-fd *drm-state*))
          (fb-id (drm-state-fb-id *drm-state*)))
      (%drm-mode-dirty-fb fd fb-id (cffi:null-pointer) 0))))
