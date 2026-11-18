;;;; src/hal/drm/package.lisp - Package Definition for DRM/KMS Driver

(defpackage :lispos.drm
  (:use :cl)
  (:export
   #:*drm-state*
   #:drm-state
   #:drm-state-fd
   #:drm-state-crtc-id
   #:drm-state-connector-id
   #:drm-state-fb-id
   #:drm-state-handle
   #:drm-state-width
   #:drm-state-height
   #:drm-state-pitch
   #:drm-state-size
   #:drm-state-sap
   #:init-drm
   #:close-drm
   #:clear-screen
   #:flush-screen
   #:draw-rect
   #:draw-string
   #:draw-char
   #:rgb
   #:argb))
