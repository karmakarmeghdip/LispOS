;;;; src/drm.lisp - Direct DRM/KMS Dumb-Buffer Graphics for LispOS

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


(defstruct drm-state
  fd
  crtc-id
  connector-id
  fb-id
  handle
  width
  height
  pitch
  size
  sap
  mmap-addr)

(defvar *drm-state* nil
  "Global active DRM state.")

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

;; Drawing primitives
(declaim (inline rgb argb))
(defun rgb (r g b)
  "Create 32-bit ARGB pixel."
  (logior #xFF000000 (ash (logand r #xFF) 16) (ash (logand g #xFF) 8) (logand b #xFF)))

(defun argb (a r g b)
  "Create 32-bit ARGB pixel with explicit alpha."
  (logior (ash (logand a #xFF) 24) (ash (logand r #xFF) 16) (ash (logand g #xFF) 8) (logand b #xFF)))

(defun flush-screen ()
  "Notify DRM KMS driver that framebuffer contents are dirty and should be transferred to display plane."
  (when *drm-state*
    (let ((fd (drm-state-fd *drm-state*))
          (fb-id (drm-state-fb-id *drm-state*)))
      (%drm-mode-dirty-fb fd fb-id (cffi:null-pointer) 0))))

(defun clear-screen (&optional (color #xFF0A0E14))
  "Clear screen to color (default dark slate)."
  (when *drm-state*
    (let* ((sap (drm-state-sap *drm-state*))
           (pitch (drm-state-pitch *drm-state*))
           (width (drm-state-width *drm-state*))
           (height (drm-state-height *drm-state*)))
      (dotimes (y height)
        (let ((row-offset (* y pitch)))
          (dotimes (x width)
            (setf (sb-sys:sap-ref-32 sap (+ row-offset (* x 4))) color))))
      (flush-screen))))


(defun draw-rect (x y w h color)
  "Draw a filled rectangle at (x, y) with dimensions (w, h) and ARGB color."
  (when *drm-state*
    (let* ((sap (drm-state-sap *drm-state*))
           (pitch (drm-state-pitch *drm-state*))
           (max-w (drm-state-width *drm-state*))
           (max-h (drm-state-height *drm-state*))
           (x1 (max 0 (min x max-w)))
           (y1 (max 0 (min y max-h)))
           (x2 (max 0 (min (+ x w) max-w)))
           (y2 (max 0 (min (+ y h) max-h))))
      (loop for cy from y1 below y2 do
        (let ((row-offset (* cy pitch)))
          (loop for cx from x1 below x2 do
            (setf (sb-sys:sap-ref-32 sap (+ row-offset (* cx 4))) color)))))))

;; Embedded 8x8 font table for ASCII characters 32..126
(defparameter *font-8x8*
  (make-array
   '(95 8) :element-type '(unsigned-byte 8)
   :initial-contents
   '((#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00) ; space
     (#x18 #x3C #x3C #x18 #x18 #x00 #x18 #x00) ; !
     (#x66 #x66 #x24 #x00 #x00 #x00 #x00 #x00) ; "
     (#x6C #x6C #xFE #x6C #xFE #x6C #x6C #x00) ; #
     (#x18 #x7E #xC0 #x7C #x06 #xFC #x18 #x00) ; $
     (#x00 #xC6 #xCC #x18 #x30 #x66 #xC6 #x00) ; %
     (#x38 #x6C #x38 #x76 #xDC #xCC #x76 #x00) ; &
     (#x30 #x30 #x60 #x00 #x00 #x00 #x00 #x00) ; '
     (#x0C #x18 #x30 #x30 #x30 #x18 #x0C #x00) ; (
     (#x30 #x18 #x0C #x0C #x0C #x18 #x30 #x00) ; )
     (#x00 #x66 #x3C #xFF #x3C #x66 #x00 #x00) ; *
     (#x00 #x18 #x18 #x7E #x18 #x18 #x00 #x00) ; +
     (#x00 #x00 #x00 #x00 #x00 #x18 #x18 #x30) ; ,
     (#x00 #x00 #x00 #x7E #x00 #x00 #x00 #x00) ; -
     (#x00 #x00 #x00 #x00 #x00 #x18 #x18 #x00) ; .
     (#x06 #x0C #x18 #x30 #x60 #xC0 #x80 #x00) ; /
     (#x7C #xC6 #xCE #xD6 #xE6 #xC6 #x7C #x00) ; 0
     (#x18 #x38 #x18 #x18 #x18 #x18 #x7E #x00) ; 1
     (#x7C #xC6 #x06 #x1C #x30 #x60 #xFE #x00) ; 2
     (#x7C #xC6 #x06 #x3C #x06 #xC6 #x7C #x00) ; 3
     (#x1C #x3C #x6C #xCC #xFE #x0C #x0C #x00) ; 4
     (#xFE #xC0 #xFC #x06 #x06 #xC6 #x7C #x00) ; 5
     (#x38 #x60 #xC0 #xFC #xC6 #xC6 #x7C #x00) ; 6
     (#xFE #xC6 #x0C #x18 #x30 #x30 #x30 #x00) ; 7
     (#x7C #xC6 #xC6 #x7C #xC6 #xC6 #x7C #x00) ; 8
     (#x7C #xC6 #xC6 #x7E #x06 #x0C #x78 #x00) ; 9
     (#x00 #x18 #x18 #x00 #x18 #x18 #x00 #x00) ; :
     (#x00 #x18 #x18 #x00 #x18 #x18 #x30 #x00) ; ;
     (#x06 #x0C #x18 #x30 #x18 #x0C #x06 #x00) ; <
     (#x00 #x00 #x7E #x00 #x7E #x00 #x00 #x00) ; =
     (#x60 #x30 #x18 #x0C #x18 #x30 #x60 #x00) ; >
     (#x7C #xC6 #x0C #x18 #x18 #x00 #x18 #x00) ; ?
     (#x7C #xC6 #xDE #xDE #xDC #xC0 #x7C #x00) ; @
     (#x38 #x6C #xC6 #xFE #xC6 #xC6 #xC6 #x00) ; A
     (#xFC #x66 #x66 #x7C #x66 #x66 #xFC #x00) ; B
     (#x3C #x66 #xC0 #xC0 #xC0 #x66 #x3C #x00) ; C
     (#xF8 #x6C #x66 #x66 #x66 #x6C #xF8 #x00) ; D
     (#xFE #x62 #x68 #x78 #x68 #x62 #xFE #x00) ; E
     (#xFE #x62 #x68 #x78 #x68 #x60 #xF0 #x00) ; F
     (#x3C #x66 #xC0 #xC0 #xCE #x66 #x3E #x00) ; G
     (#xC6 #xC6 #xC6 #xFE #xC6 #xC6 #xC6 #x00) ; H
     (#x3C #x18 #x18 #x18 #x18 #x18 #x3C #x00) ; I
     (#x1E #x0C #x0C #x0C #xCC #xCC #x78 #x00) ; J
     (#xE6 #x66 #x6C #x78 #x6C #x66 #xE6 #x00) ; K
     (#xF0 #x60 #x60 #x60 #x62 #x66 #xFE #x00) ; L
     (#xC6 #xEE #xFE #xFE #xD6 #xC6 #xC6 #x00) ; M
     (#xC6 #xE6 #xF6 #xDE #xCE #xC6 #xC6 #x00) ; N
     (#x7C #xC6 #xC6 #xC6 #xC6 #xC6 #x7C #x00) ; O
     (#xFC #x66 #x66 #x7C #x60 #x60 #xF0 #x00) ; P
     (#x7C #xC6 #xC6 #xC6 #xD6 #xDE #x7C #x0E) ; Q
     (#xFC #x66 #x66 #x7C #x6C #x66 #xE6 #x00) ; R
     (#x7C #xC6 #x60 #x38 #x0C #xC6 #x7C #x00) ; S
     (#x7E #x18 #x18 #x18 #x18 #x18 #x18 #x00) ; T
     (#xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #x7C #x00) ; U
     (#xC6 #xC6 #xC6 #xC6 #xC6 #x6C #x38 #x00) ; V
     (#xC6 #xC6 #xC6 #xD6 #xD6 #xFE #x6C #x00) ; W
     (#xC6 #xC6 #x6C #x38 #x6C #xC6 #xC6 #x00) ; X
     (#x66 #x66 #x66 #x3C #x18 #x18 #x3C #x00) ; Y
     (#xFE #xC6 #x0C #x18 #x30 #x62 #xFE #x00) ; Z
     (#x3C #x30 #x30 #x30 #x30 #x30 #x3C #x00) ; [
     (#xC0 #x60 #x30 #x18 #x0C #x06 #x02 #x00) ; \
     (#x3C #x0C #x0C #x0C #x0C #x0C #x3C #x00) ; ]
     (#x10 #x38 #x6C #xC6 #x00 #x00 #x00 #x00) ; ^
     (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF) ; _
     (#x30 #x18 #x0C #x00 #x00 #x00 #x00 #x00) ; `
     (#x00 #x00 #x78 #x0C #x7C #xCC #x76 #x00) ; a
     (#xE0 #x60 #x7C #x66 #x66 #x66 #xDC #x00) ; b
     (#x00 #x00 #x78 #xCC #xC0 #xCC #x78 #x00) ; c
     (#x1C #x0C #x7C #xCC #xCC #xCC #x76 #x00) ; d
     (#x00 #x00 #x78 #xCC #xFC #xC0 #x78 #x00) ; e
     (#x38 #x6C #x60 #xF0 #x60 #x60 #xF0 #x00) ; f
     (#x00 #x00 #x76 #xCC #xCC #x7C #x0C #xF8) ; g
     (#xE0 #x60 #x6C #x76 #x66 #x66 #xE6 #x00) ; h
     (#x18 #x00 #x38 #x18 #x18 #x18 #x3C #x00) ; i
     (#x06 #x00 #x0E #x06 #x06 #x66 #x66 #x3C) ; j
     (#xE0 #x60 #x66 #x6C #x78 #x6C #xE6 #x00) ; k
     (#x38 #x18 #x18 #x18 #x18 #x18 #x3C #x00) ; l
     (#x00 #x00 #xEC #xFE #xD6 #xD6 #xD6 #x00) ; m
     (#x00 #x00 #xDC #x66 #x66 #x66 #x66 #x00) ; n
     (#x00 #x00 #x78 #xCC #xCC #xCC #x78 #x00) ; o
     (#x00 #x00 #xDC #x66 #x66 #x7C #x60 #xF0) ; p
     (#x00 #x00 #x76 #xCC #xCC #x7C #x0C #x1E) ; q
     (#x00 #x00 #xDC #x76 #x60 #x60 #xF0 #x00) ; r
     (#x00 #x00 #x7C #xC0 #x78 #x0C #xF8 #x00) ; s
     (#x10 #x30 #x7C #x30 #x30 #x34 #x18 #x00) ; t
     (#x00 #x00 #xCC #xCC #xCC #xCC #x76 #x00) ; u
     (#x00 #x00 #xC6 #xC6 #xC6 #x6C #x38 #x00) ; v
     (#x00 #x00 #xC6 #xD6 #xD6 #xFE #x6C #x00) ; w
     (#x00 #x00 #xC6 #x6C #x38 #x6C #xC6 #x00) ; x
     (#x00 #x00 #xC6 #xC6 #xC6 #x7E #x06 #xFC) ; y
     (#x00 #x00 #x7E #x4C #x18 #x32 #x7E #x00) ; z
     (#x0E #x18 #x18 #x70 #x18 #x18 #x0E #x00) ; {
     (#x18 #x18 #x18 #x18 #x18 #x18 #x18 #x18) ; |
     (#x70 #x18 #x18 #x0E #x18 #x18 #x70 #x00) ; }
     (#x76 #xDC #x00 #x00 #x00 #x00 #x00 #x00) ; ~
     )))

(defun draw-char (x y char fg-color &optional bg-color (scale 2))
  "Draw an ASCII character scaled by scale (default 2 for 16x16 pixels)."
  (when *drm-state*
    (let* ((code (char-code char))
           (idx (if (and (>= code 32) (<= code 126)) (- code 32) 0))
           (sap (drm-state-sap *drm-state*))
           (pitch (drm-state-pitch *drm-state*))
           (max-w (drm-state-width *drm-state*))
           (max-h (drm-state-height *drm-state*)))
      (dotimes (row 8)
        (let ((row-bits (aref *font-8x8* idx row)))
          (dotimes (col 8)
            (let ((setp (logbitp (- 7 col) row-bits))
                  (px (+ x (* col scale)))
                  (py (+ y (* row scale))))
              (when (and (< px max-w) (< py max-h))
                (if setp
                    (draw-rect px py scale scale fg-color)
                    (when bg-color
                      (draw-rect px py scale scale bg-color)))))))))))

(defun draw-string (x y string fg-color &optional bg-color (scale 2))
  "Draw string with spacing."
  (let ((cur-x x))
    (loop for ch across string do
      (draw-char cur-x y ch fg-color bg-color scale)
      (incf cur-x (* 8 scale)))))
