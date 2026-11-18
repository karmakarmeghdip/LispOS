;;;; src/hal/drm/framebuffer.lisp - Framebuffer Drawing & Blitting Primitives

(in-package :lispos.drm)

(deftype pixel-argb ()
  '(unsigned-byte 32))

;; Drawing primitives
(declaim (inline rgb argb))
(declaim (ftype (function (fixnum fixnum fixnum) pixel-argb) rgb))
(defun rgb (r g b)
  "Create 32-bit ARGB pixel."
  (declare (type fixnum r g b)
           (optimize (speed 3) (safety 1)))
  (logior #xFF000000 (ash (logand r #xFF) 16) (ash (logand g #xFF) 8) (logand b #xFF)))

(declaim (ftype (function (fixnum fixnum fixnum fixnum) pixel-argb) argb))
(defun argb (a r g b)
  "Create 32-bit ARGB pixel with explicit alpha."
  (declare (type fixnum a r g b)
           (optimize (speed 3) (safety 1)))
  (logior (ash (logand a #xFF) 24) (ash (logand r #xFF) 16) (ash (logand g #xFF) 8) (logand b #xFF)))

(declaim (ftype (function (&optional pixel-argb) null) clear-screen))
(defun clear-screen (&optional (color #xFF0A0E14))
  "Clear screen to color (default dark slate)."
  (when *drm-state*
    (let* ((sap (drm-state-sap *drm-state*))
           (pitch (drm-state-pitch *drm-state*))
           (width (drm-state-width *drm-state*))
           (height (drm-state-height *drm-state*)))
      (declare (type sb-sys:system-area-pointer sap)
               (type (unsigned-byte 32) pitch)
               (type (unsigned-byte 16) width height)
               (type pixel-argb color)
               (optimize (speed 3) (safety 1)))
      (dotimes (y height)
        (let ((row-offset (* y pitch)))
          (dotimes (x width)
            (setf (sb-sys:sap-ref-32 sap (+ row-offset (* x 4))) color))))
      (flush-screen)))
  nil)

(declaim (ftype (function (fixnum fixnum fixnum fixnum pixel-argb) null) draw-rect))
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
      (declare (type sb-sys:system-area-pointer sap)
               (type (unsigned-byte 32) pitch)
               (type (unsigned-byte 16) max-w max-h)
               (type fixnum x1 y1 x2 y2)
               (type pixel-argb color)
               (optimize (speed 3) (safety 1)))
      (loop for cy from y1 below y2 do
        (let ((row-offset (* cy pitch)))
          (loop for cx from x1 below x2 do
            (setf (sb-sys:sap-ref-32 sap (+ row-offset (* cx 4))) color))))))
  nil)

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
           (max-w (drm-state-width *drm-state*))
           (max-h (drm-state-height *drm-state*)))
      (declare (type fixnum code idx x y scale)
               (type (unsigned-byte 16) max-w max-h))
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
    (declare (type fixnum cur-x x y scale))
    (loop for ch across string do
      (draw-char cur-x y ch fg-color bg-color scale)
      (incf cur-x (* 8 scale)))))
