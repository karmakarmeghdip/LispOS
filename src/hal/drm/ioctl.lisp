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

(defun init-drm (&optional (device-path lispos.paths:+drm-card0+))
  "Initialize KMS framebuffer on device-path."
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
                           (modes-ptr (cffi:mem-ref (cffi:inc-pointer conn 40) :pointer))
                           ;; First mode is the preferred mode
                           (mode-info-ptr modes-ptr)
                           (width (cffi:mem-ref (cffi:inc-pointer mode-info-ptr 4) :uint16))
                           (height (cffi:mem-ref (cffi:inc-pointer mode-info-ptr 14) :uint16)))
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
                                          (let ((crtc-ret (%drm-mode-set-crtc fd crtc-id fb-id 0 0 conns-arr 1 mode-info-ptr)))
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
          (%drm-mode-free-resources res))))))

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
