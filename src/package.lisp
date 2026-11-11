;;;; src/package.lisp - Package Definitions for LispOS

(defpackage :lispos.paths
  (:use :cl)
  (:export
   #:+root+
   #:+system-root+
   #:+system-core+
   #:+system-libraries+
   #:+system-source+
   #:+system-configuration+
   #:+system-runtime+
   #:+proc-root+
   #:+sys-root+
   #:+dev-root+
   #:+drm-card0+
   #:+serial-console+
   #:+applications-root+
   #:+library-root+
   #:+users-root+
   #:+admin-home+
   #:setup-logical-pathnames
   #:ensure-system-hierarchy))

(defpackage :lispos.sys
  (:use :cl)
  (:export
   #:mount
   #:umount
   #:ioctl
   #:setup-network-interface
   #:install-zombie-reaper
   #:reap-zombies
   #:sync-filesystem
   #:poweroff
   #:reboot
   #:load-kernel-module
   #:+ms-rdonly+
   #:+ms-nosuid+
   #:+ms-nodev+
   #:+ms-noexec+
   #:+ms-bind+
   #:+ms-move+
   #:+mnt-detach+))

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


(defpackage :lispos.input
  (:use :cl)
  (:export
   #:start-input-loop
   #:stop-input-loop
   #:register-key-hook
   #:register-pointer-hook
   #:poll-input-event
   #:enqueue-input-event
   #:scancode->keysym
   #:input-event
   #:make-input-event
   #:input-event-type
   #:input-event-key
   #:input-event-char
   #:input-event-modifiers
   #:modifier-active-p))

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
   #:init-display
   #:close-display
   #:clear-display
   #:fill-rect
   #:mark-dirty
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

(defpackage :lispos.buffer
  (:use :cl)
  (:export
   #:*buffers*
   #:*current-buffer*
   #:buffer
   #:buffer-p
   #:make-buffer
   #:buffer-id
   #:buffer-name
   #:buffer-mode
   #:buffer-engine
   #:buffer-file-path
   #:buffer-modified-p
   #:create-buffer
   #:find-buffer
   #:kill-buffer
   #:list-buffers
   #:buffer-content-lines
   #:buffer-cursor-position
   #:buffer-insert-char
   #:buffer-handle-key))

(defpackage :lispos.picker
  (:use :cl)
  (:export
   #:*active-picker*
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

(defpackage :lispos.shell
  (:use :cl)
  (:export
   #:*default-prompt*
   #:*prompt-fn*
   #:default-lispos-prompt
   #:get-cwd-namestring
   #:*shell-commands*
   #:define-shell-command
   #:get-shell-command
   #:tokenize-shell-line
   #:resolve-path
   #:evaluate-shell-line
   #:shell-engine
   #:shell-engine-p
   #:make-shell-engine
   #:engine-buffer
   #:engine-point
   #:engine-prompt
   #:engine-history
   #:engine-history-index
   #:engine-scratch-buffer
   #:engine-scratch-point
   #:engine-output-history
   #:engine-package
   #:engine-last-eval-values
   #:engine-last-stdout
   #:engine-last-stderr
   #:engine-last-error
   #:engine-last-status
   #:buffer-text
   #:buffer-point
   #:set-buffer-text
   #:set-buffer-point
   #:handle-key
   #:handle-char
   #:handle-backspace
   #:handle-delete
   #:handle-left
   #:handle-right
   #:handle-home
   #:handle-end
   #:handle-history-up
   #:handle-history-down
   #:handle-enter
   #:feed-input
   #:balanced-parentheses-p
   #:submit-buffer
   #:evaluate-string
   #:output-history-lines
   #:render-prompt-and-buffer
   #:run-shell-tests))

(defpackage :lispos.repl
  (:use :cl :lispos.shell)
  (:export
   #:start-serial-repl
   #:start-slynk-server
   ;; Re-export shell engine APIs from lispos.shell
   #:*default-prompt*
   #:*prompt-fn*
   #:default-lispos-prompt
   #:get-cwd-namestring
   #:*shell-commands*
   #:define-shell-command
   #:get-shell-command
   #:tokenize-shell-line
   #:resolve-path
   #:evaluate-shell-line
   #:shell-engine
   #:shell-engine-p
   #:make-shell-engine
   #:engine-buffer
   #:engine-point
   #:engine-prompt
   #:engine-history
   #:engine-output-history
   #:engine-last-eval-values
   #:engine-last-stdout
   #:engine-last-stderr
   #:engine-last-error
   #:engine-last-status
   #:buffer-text
   #:buffer-point
   #:set-buffer-text
   #:set-buffer-point
   #:handle-key
   #:handle-char
   #:feed-input
   #:balanced-parentheses-p
   #:submit-buffer
   #:evaluate-string
   #:output-history-lines
   #:render-prompt-and-buffer
   #:run-shell-tests))

(defpackage :lispos.sandbox
  (:use :cl)
  (:export
   #:safe-eval
   #:make-sandbox-env))

(defpackage :lispos.init
  (:use :cl)
  (:export
   #:boot-os))
