;;;; lispos.asd - ASDF System Definition for LispOS Monolithic Userland

(asdf:defsystem :lispos
  :description "Monolithic Common Lisp Linux Userland (LispOS)"
  :author "LispOS Team"
  :license "MIT"
  :version "0.2.0"
  :depends-on (:cffi :slynk :usocket :sb-posix :cl-freetype2)
  :serial t
  :components
  ((:module "core"
    :pathname "src/core"
    :serial t
    :components ((:file "package")
                 (:file "paths")
                 (:file "syscalls")))
   (:module "hal"
    :pathname "src/hal"
    :serial t
    :components ((:file "drm/package")
                 (:file "drm/ioctl")
                 (:file "drm/framebuffer")
                 (:file "input/package")
                 (:file "input/keycodes")
                 (:file "input/evdev")))
   (:module "graphics"
    :pathname "src/graphics"
    :serial t
    :components ((:file "package")
                 (:file "surface")
                 (:file "font")))
   (:module "shell"
    :pathname "src/shell"
    :serial t
    :components ((:file "package")
                 (:file "state")
                 (:file "parser")
                 (:file "line-editor")
                 (:file "history")
                 (:file "commands/registry")
                 (:file "commands/filesystem")
                 (:file "commands/system")
                 (:file "engine")))
   (:module "ui"
    :pathname "src/ui"
    :serial t
    :components ((:file "package")
                 (:file "buffer")
                 (:file "editor")
                 (:file "window-tree")
                 (:file "picker")
                 (:file "terminal")))
   (:module "supervisor"
    :pathname "src/supervisor"
    :serial t
    :components ((:file "package")
                 (:file "sandbox")
                 (:file "repl")
                 (:file "init")))))
