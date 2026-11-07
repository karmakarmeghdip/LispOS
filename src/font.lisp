;;;; src/font.lisp - TrueType Monospace Font Engine for LispOS using FreeType 2

(in-package :lispos.font)

(pushnew #P"/System/Libraries/" cffi:*foreign-library-directories* :test #'equal)
(pushnew #P"/usr/lib/" cffi:*foreign-library-directories* :test #'equal)

(declaim (inline blend-pixel))

(defun blend-pixel (dst fg alpha)
  "Alpha-blend foreground ARGB onto destination ARGB given 8-bit alpha (0-255)."
  (declare (type (unsigned-byte 32) dst fg)
           (type (unsigned-byte 8) alpha)
           (optimize (speed 3) (safety 0)))
  (cond
    ((= alpha 0) dst)
    ((= alpha 255) fg)
    (t
     (let* ((inv-a (- 255 alpha))
            (dr (logand (ash dst -16) #xFF))
            (dg (logand (ash dst -8) #xFF))
            (db (logand dst #xFF))
            (fr (logand (ash fg -16) #xFF))
            (fg-g (logand (ash fg -8) #xFF))
            (fb (logand fg #xFF))
            (out-r (ash (+ (* fr alpha) (* dr inv-a)) -8))
            (out-g (ash (+ (* fg-g alpha) (* dg inv-a)) -8))
            (out-b (ash (+ (* fb alpha) (* db inv-a)) -8)))
       (logior #xFF000000
               (ash out-r 16)
               (ash out-g 8)
               out-b)))))

(defstruct glyph-entry
  (width 0 :type fixnum)
  (rows 0 :type fixnum)
  (left 0 :type fixnum)
  (top 0 :type fixnum)
  (advance-x 0 :type fixnum)
  (buffer nil :type (or null (simple-array (unsigned-byte 8) (*)))))

(defstruct font
  (face nil)
  (size 16 :type fixnum)
  (char-width 10 :type fixnum)
  (char-height 20 :type fixnum)
  (line-height 22 :type fixnum)
  (baseline 16 :type fixnum)
  (glyph-cache (make-hash-table :test #'eql)))

(defvar *font* nil
  "Default loaded font instance.")

(defparameter *font-search-paths*
  '("/Library/Fonts/CascadiaCodeNF.ttf"
    "/Library/Fonts/IosevkaNerdFontMono-Regular.ttf"
    "/usr/share/fonts/TTF/CascadiaCodeNF.ttf"
    "/usr/share/fonts/TTF/CascadiaCode.ttf"
    "/usr/share/fonts/TTF/IosevkaNerdFontMono-Regular.ttf"))

(defun find-font-file ()
  "Find an available monospace TrueType font file."
  (dolist (path *font-search-paths* nil)
    (when (probe-file path)
      (return-from find-font-file path))))

(defun load-glyph-entry (face char-code)
  "Render a glyph using FreeType 2 and copy bitmap data into an internal glyph-entry."
  (handler-case
      (progn
        (ft2:load-char face char-code (list :render))
        (let* ((glyph (ft2::ft-face-glyph face))
               (bitmap (ft2-types:ft-glyphslot-bitmap glyph))
               (width (ft2-types:ft-bitmap-width bitmap))
               (rows (ft2-types:ft-bitmap-rows bitmap))
               (pitch (ft2-types:ft-bitmap-pitch bitmap))
               (left (ft2-types:ft-glyphslot-bitmap-left glyph))
               (top (ft2-types:ft-glyphslot-bitmap-top glyph))
               (advance (ash (ft2-types:ft-vector-x (ft2-types:ft-glyphslot-advance glyph)) -6))
               (raw-buf (ft2-types:ft-bitmap-buffer bitmap)))
          (if (and (> width 0) (> rows 0) (not (cffi:null-pointer-p raw-buf)))
              (let ((buf (make-array (* width rows) :element-type '(unsigned-byte 8))))
                (dotimes (r rows)
                  (let ((src-row-start (* r pitch))
                        (dst-row-start (* r width)))
                    (dotimes (c width)
                      (setf (aref buf (+ dst-row-start c))
                            (sb-sys:sap-ref-8 raw-buf (+ src-row-start c))))))
                (make-glyph-entry :width width
                                  :rows rows
                                  :left left
                                  :top top
                                  :advance-x (if (> advance 0) advance width)
                                  :buffer buf))
              ;; Whitespace / non-printing glyph (e.g. space)
              (make-glyph-entry :width 0
                                :rows 0
                                :left 0
                                :top 0
                                :advance-x (if (> advance 0) advance 10)
                                :buffer nil))))
    (error ()
      nil)))

(defun get-glyph (font char)
  "Retrieve or render glyph-entry for character."
  (let* ((code (char-code char))
         (cache (font-glyph-cache font))
         (cached (gethash code cache)))
    (if cached
        cached
        (let ((entry (load-glyph-entry (font-face font) code)))
          (setf (gethash code cache) (or entry :missing))
          entry))))

(defun init-font (&key (font-path nil) (font-size 16) (dpi 96))
  "Initialize FreeType 2 font face and compute metrics."
  ;; Always reinitialize the FreeType foreign handle on boot to replace stale pointer
  (setf ft2:*library* (ft2:make-freetype))
  (let ((path (or font-path (find-font-file))))
    (unless path
      (error "[FONT] No suitable TrueType font found."))
    (format t "[FONT] Loading font ~A (size ~Apt, ~Adpi)...~%" path font-size dpi)
    (let ((face (ft2:new-face path)))
      (ft2:set-char-size face (* font-size 64) 0 dpi dpi)
      (let* ((font-inst (make-font :face face :size font-size))
             ;; Probe 'M' for standard monospace cell dimensions
             (m-glyph (get-glyph font-inst #\M))
             (char-w (if (and m-glyph (> (glyph-entry-advance-x m-glyph) 0))
                         (glyph-entry-advance-x m-glyph)
                         10))
             (line-h (round (* font-size 1.35)))
             (baseline (round (* font-size 1.05))))
        (setf (font-char-width font-inst) char-w
              (font-char-height font-inst) line-h
              (font-line-height font-inst) line-h
              (font-baseline font-inst) baseline)
        ;; Pre-cache common ASCII glyphs and lambda
        (loop for c from 32 to 126 do
          (get-glyph font-inst (code-char c)))
        (get-glyph font-inst (code-char #x03BB)) ; Greek lambda 'λ'
        (setf *font* font-inst)
        font-inst))))

(defun draw-char-at (display font x y char fg-color &optional bg-color)
  "Draw single character with subpixel anti-aliasing into display backbuffer at (x, y)."
  (declare (type fixnum x y)
           (type (unsigned-byte 32) fg-color)
           (optimize (speed 3) (safety 1)))
  (let* ((cw (font-char-width font))
         (lh (font-line-height font))
         (bl (font-baseline font)))
    ;; Fill background if requested
    (when bg-color
      (lispos.display:fill-rect display x y cw lh bg-color))
    (let ((glyph (get-glyph font char)))
      (when (and glyph (not (eq glyph :missing)))
        (let ((gw (glyph-entry-width glyph))
              (gh (glyph-entry-rows glyph))
              (gx (+ x (glyph-entry-left glyph)))
              (gy (+ y (- bl (glyph-entry-top glyph))))
              (buf (glyph-entry-buffer glyph))
              (backbuffer (lispos.display:display-backbuffer display))
              (disp-w (lispos.display:display-width display))
              (disp-h (lispos.display:display-height display)))
          (when (and buf (> gw 0) (> gh 0))
            (dotimes (r gh)
              (let ((py (+ gy r)))
                (when (and (>= py 0) (< py disp-h))
                  (let ((dst-row-offset (* py disp-w))
                        (src-row-offset (* r gw)))
                    (dotimes (c gw)
                      (let ((px (+ gx c)))
                        (when (and (>= px 0) (< px disp-w))
                          (let* ((alpha (aref buf (+ src-row-offset c)))
                                 (dst-idx (+ dst-row-offset px))
                                 (dst-pixel (aref backbuffer dst-idx)))
                            (setf (aref backbuffer dst-idx)
                                  (blend-pixel dst-pixel fg-color alpha))))))))))
            (lispos.display:mark-dirty display gx gy gw gh))))))
  (font-char-width font))

(defun draw-text (display font x y string fg-color &optional bg-color)
  "Draw text string at (x, y) with anti-aliasing."
  (let ((cur-x x))
    (loop for ch across string do
      (let ((adv (draw-char-at display font cur-x y ch fg-color bg-color)))
        (incf cur-x adv)))
    cur-x))

(defun measure-text (font string)
  "Return width in pixels of string."
  (* (font-char-width font) (length string)))
