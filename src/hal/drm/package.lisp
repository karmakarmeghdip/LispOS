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
   #:drm-state-mmap-addr
   #:drm-mode-info
   #:drm-mode-info-p
   #:drm-mode-info-index
   #:drm-mode-info-name
   #:drm-mode-info-width
   #:drm-mode-info-height
   #:drm-mode-info-vrefresh
   #:drm-mode-info-flags
   #:drm-mode-info-type
   #:drm-mode-info-preferred-p
   #:drm-mode-info-current-p
   #:*mode-changed-hooks*
   #:parse-resolution-string
   #:detect-boot-resolution
   #:list-modes
   #:find-mode
   #:set-mode
   #:init-drm
   #:close-drm
   #:clear-screen
   #:flush-screen
   #:draw-rect
   #:draw-string
   #:draw-char
   #:rgb
   #:argb))
