;;;; src/ui/package.lisp - Package Definitions for UI & Windowing Subsystem

(defpackage :lispos.buffer
  (:use :cl)
  (:export
   ;; Base Buffer Class & Predicates
   #:buffer
   #:buffer-p
   #:buffer-id
   #:buffer-name
   #:buffer-modified-p
   #:buffer-read-only-p
   #:buffer-file-path
   #:buffer-engine
   #:buffer-mode

   ;; Minimal Polymorphic Buffer Protocol
   #:buffer-content-lines
   #:buffer-cursor-position
   #:buffer-mode-name
   #:buffer-handle-key
   #:buffer-modeline-details
   #:save-buffer

   ;; Concrete Buffer Classes
   #:shell-buffer
   #:editor-buffer
   #:file-buffer
   #:scratch-buffer

   ;; Editor Slots & Accessors
   #:editor-buffer-lines
   #:editor-buffer-cursor-row
   #:editor-buffer-cursor-col
   #:editor-buffer-helix-mode
   #:editor-buffer-command-input
   #:editor-buffer-status-message
   #:editor-buffer-clipboard
   #:editor-buffer-selection-anchor-row
   #:editor-buffer-selection-anchor-col
   #:editor-buffer-undo-stack
   #:editor-buffer-redo-stack
   #:editor-buffer-leader-active-p
   #:editor-buffer-pending-command
   #:file-buffer-path

   ;; Buffer Lifecycle & Registry
   #:*buffers*
   #:*current-buffer*
   #:list-buffers
   #:find-buffer
   #:kill-buffer
   #:register-buffer
   #:create-buffer
   #:open-file-in-buffer
   #:create-scratch-buffer))

(defpackage :lispos.picker
  (:use :cl)
  (:export
   #:*active-picker*
   #:picker-item
   #:picker-item-p
   #:make-picker-item
   #:picker-item-title
   #:picker-item-subtitle
   #:picker-item-badge
   #:picker-item-value
   #:picker
   #:picker-p
   #:make-picker
   #:picker-title
   #:picker-items
   #:picker-filter
   #:picker-selected-index
   #:picker-on-select
   #:picker-visible-p
   #:picker-width
   #:picker-item-height
   #:picker-max-visible
   #:open-picker
   #:close-picker
   #:picker-handle-key
   #:picker-filtered-items
   #:fuzzy-match-score
   #:open-buffer-picker
   #:open-file-picker
   #:open-command-palette))

(defpackage :lispos.window
  (:use :cl)
  (:export
   #:*root-window*
   #:*focused-window*
   #:window
   #:window-p
   #:make-window
   #:window-id
   #:window-buffer
   #:window-x
   #:window-y
   #:window-width
   #:window-height
   #:window-scroll-row
   #:window-scroll-col
   #:window-split-type
   #:window-split-ratio
   #:window-first-child
   #:window-second-child
   #:window-parent
   #:leaf-window-p
   #:split-window-horizontally
   #:split-window-vertically
   #:close-window
   #:focus-window
   #:focus-direction
   #:all-leaf-windows
   #:layout-windows
   #:active-window-modeline-info))

(defpackage :lispos.terminal
  (:use :cl)
  (:export
   #:*terminal*
   #:terminal
   #:terminal-p
   #:make-terminal
   #:terminal-engine
   #:terminal-cols
   #:terminal-rows
   #:terminal-font
   #:terminal-display
   #:terminal-active-picker
   #:init-terminal
   #:render-terminal
   #:terminal-handle-key
   #:terminal-event-loop
   #:start-terminal-thread
   #:stop-terminal-thread))
