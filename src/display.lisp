;;;; src/display.lisp - Double-Buffered Display Subsystem for LispOS

(in-package :lispos.display)

(declaim (inline rgb argb))

(defun rgb (r g b)
  "Construct 32-bit ARGB pixel value from red, green, blue (0-255)."
  (logior #xFF000000
          (ash (logand r #xFF) 16)
          (ash (logand g #xFF) 8)
          (logand b #xFF)))

(defun argb (a r g b)
  "Construct 32-bit ARGB pixel value with explicit alpha channel."
  (logior (ash (logand a #xFF) 24)
          (ash (logand r #xFF) 16)
          (ash (logand g #xFF) 8)
          (logand b #xFF)))

(defstruct (display (:constructor %make-display))
  (width 1280 :type fixnum)
  (height 800 :type fixnum)
  (pitch 5120 :type fixnum)
  (backbuffer nil :type (or null (simple-array (unsigned-byte 32) (*))))
  (drm-state nil)
  ;; Damage tracking bounding box
  (dirty-x1 0 :type fixnum)
  (dirty-y1 0 :type fixnum)
  (dirty-x2 0 :type fixnum)
  (dirty-y2 0 :type fixnum)
  (dirty-p nil :type boolean))

(defvar *display* nil
  "Active primary display instance.")

(defun mark-dirty (display x y w h)
  "Expand the dirty bounding box to include the specified rectangle."
  (declare (type display display)
           (type fixnum x y w h)
           (optimize (speed 3) (safety 1)))
  (let ((x2 (+ x w))
        (y2 (+ y h))
        (max-w (display-width display))
        (max-h (display-height display)))
    (let ((cx1 (max 0 (min x max-w)))
          (cy1 (max 0 (min y max-h)))
          (cx2 (max 0 (min x2 max-w)))
          (cy2 (max 0 (min y2 max-h))))
      (when (and (< cx1 cx2) (< cy1 cy2))
        (if (display-dirty-p display)
            (setf (display-dirty-x1 display) (min (display-dirty-x1 display) cx1)
                  (display-dirty-y1 display) (min (display-dirty-y1 display) cy1)
                  (display-dirty-x2 display) (max (display-dirty-x2 display) cx2)
                  (display-dirty-y2 display) (max (display-dirty-y2 display) cy2))
            (setf (display-dirty-x1 display) (the fixnum cx1)
                  (display-dirty-y1 display) (the fixnum cy1)
                  (display-dirty-x2 display) (the fixnum cx2)
                  (display-dirty-y2 display) (the fixnum cy2)
                  (display-dirty-p display) t))))))

(defun mark-all-dirty (display)
  "Mark entire display as damaged for a full redraw."
  (setf (display-dirty-x1 display) 0
        (display-dirty-y1 display) 0
        (display-dirty-x2 display) (display-width display)
        (display-dirty-y2 display) (display-height display)
        (display-dirty-p display) t))

(defun init-display (&optional (drm-state lispos.drm:*drm-state*))
  "Initialize double-buffered display on top of DRM KMS."
  (let* ((drm (or drm-state
                  (and (not lispos.drm:*drm-state*) (lispos.drm:init-drm))
                  lispos.drm:*drm-state*))
         (w (if drm (lispos.drm:drm-state-width drm) 1280))
         (h (if drm (lispos.drm:drm-state-height drm) 800))
         (pitch (if drm (lispos.drm:drm-state-pitch drm) (* w 4)))
         (buf (make-array (* w h) :element-type '(unsigned-byte 32) :initial-element #xFF1E1E2E)))
    (setf *display*
          (%make-display :width w
                         :height h
                         :pitch pitch
                         :backbuffer buf
                         :drm-state drm
                         :dirty-x1 0
                         :dirty-y1 0
                         :dirty-x2 w
                         :dirty-y2 h
                         :dirty-p t))
    *display*))

(defun close-display (&optional (display *display*))
  "Close display."
  (when display
    (setf (display-backbuffer display) nil)
    (when (eq display *display*)
      (setf *display* nil))))

(defun clear-display (display &optional (color #xFF1E1E2E))
  "Clear backbuffer to color and mark whole screen dirty."
  (declare (type display display)
           (type (unsigned-byte 32) color)
           (optimize (speed 3) (safety 1)))
  (let ((buf (display-backbuffer display)))
    (when buf
      (fill buf color)
      (mark-all-dirty display))))

(defun fill-rect (display x y w h color)
  "Fill a rectangle with ARGB color in the RAM backbuffer."
  (declare (type display display)
           (type fixnum x y w h)
           (type (unsigned-byte 32) color)
           (optimize (speed 3) (safety 1)))
  (let* ((buf (display-backbuffer display))
         (max-w (display-width display))
         (max-h (display-height display)))
    (when buf
      (let ((x1 (max 0 (min x max-w)))
            (y1 (max 0 (min y max-h)))
            (x2 (max 0 (min (+ x w) max-w)))
            (y2 (max 0 (min (+ y h) max-h))))
        (when (and (< x1 x2) (< y1 y2))
          (loop for cy from y1 below y2 do
            (let ((row-start (* cy max-w)))
              (loop for cx from x1 below x2 do
                (setf (aref buf (+ row-start cx)) color))))
          (mark-dirty display x1 y1 (- x2 x1) (- y2 y1)))))))

(defun scroll-display (display dy &optional (bg-color #xFF1E1E2E))
  "Scroll display vertically by dy pixels (positive = move content up).
   Clears newly exposed lines at the bottom."
  (declare (type display display)
           (type fixnum dy)
           (type (unsigned-byte 32) bg-color)
           (optimize (speed 3) (safety 1)))
  (let* ((buf (display-backbuffer display))
         (w (display-width display))
         (h (display-height display)))
    (when (and buf (> dy 0) (< dy h))
      (let ((pixels-to-move (* (- h dy) w))
            (offset (* dy w)))
        (replace buf buf :start1 0 :start2 offset :end2 (+ offset pixels-to-move))
        (fill buf bg-color :start (* (- h dy) w))
        (mark-all-dirty display)))))

(defun present-display (&optional (display *display*))
  "Blit damaged region of RAM backbuffer to DRM KMS framebuffer and flush to hardware."
  (declare (optimize (speed 3) (safety 1)))
  (let ((disp (or display *display*)))
    (when (and disp (display-dirty-p disp))
      (let* ((drm (or (display-drm-state disp) lispos.drm:*drm-state*))
             (buf (display-backbuffer disp)))
        (when (and drm buf)
          (let* ((sap (lispos.drm:drm-state-sap drm))
                 (disp-w (display-width disp))
                 (pitch (display-pitch disp))
                 (x1 (display-dirty-x1 disp))
                 (y1 (display-dirty-y1 disp))
                 (x2 (display-dirty-x2 disp))
                 (y2 (display-dirty-y2 disp)))
            (declare (type fixnum disp-w pitch x1 y1 x2 y2))
            (when (and (< x1 x2) (< y1 y2))
              (loop for cy of-type fixnum from y1 below y2 do
                (let ((row-buf-start (* cy disp-w))
                      (row-sap-start (* cy pitch)))
                  (declare (type fixnum row-buf-start row-sap-start))
                  (loop for cx of-type fixnum from x1 below x2 do
                    (let ((pixel (aref buf (+ row-buf-start cx)))
                          (byte-offset (+ row-sap-start (* cx 4))))
                      (setf (sb-sys:sap-ref-32 sap byte-offset) pixel))))))
              (lispos.drm:flush-screen)))
          ;; Reset damage tracker
          (setf (display-dirty-p disp) nil
                (display-dirty-x1 disp) 0
                (display-dirty-y1 disp) 0
                (display-dirty-x2 disp) 0
                (display-dirty-y2 disp) 0)
          t))))
