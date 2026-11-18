;;;; src/graphics/package.lisp - Package Definitions for Graphics Subsystem

(defpackage :lispos.display
  (:use :cl)
  (:export
   #:*display*
   #:display
   #:display-p
   #:make-display
   #:display-width
   #:display-height
   #:display-pitch
   #:display-backbuffer
   #:display-drm-state
   #:resize-display
   #:init-display
   #:close-display
   #:clear-display
   #:fill-rect
   #:mark-dirty
   #:mark-all-dirty
   #:present-display
   #:rgb
   #:argb
   #:scroll-display))

(defpackage :lispos.font
  (:use :cl)
  (:export
   #:*font*
   #:font
   #:font-p
   #:make-font
   #:font-face
   #:font-size
   #:font-char-width
   #:font-char-height
   #:font-line-height
   #:font-baseline
   #:init-font
   #:render-glyph
   #:draw-text
   #:draw-char-at
   #:measure-text
   #:blend-pixel))
