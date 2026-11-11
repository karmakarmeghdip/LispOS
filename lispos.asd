;;;; lispos.asd - ASDF System Definition for LispOS Monolithic Userland

(asdf:defsystem :lispos
  :description "Monolithic Common Lisp Linux Userland (LispOS)"
  :author "LispOS Team"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cffi :slynk :usocket :sb-posix :cl-freetype2)
  :serial t
  :components
  ((:file "src/package")
   (:file "src/paths")
   (:file "src/syscalls")
   (:file "src/drm")
   (:file "src/display")
   (:file "src/font")
   (:file "src/input")
   (:file "src/shell")
   (:file "src/commands/filesystem")
   (:file "src/commands/system")
   (:file "src/buffer")
   (:file "src/picker")
   (:file "src/window")
   (:file "src/terminal")
   (:file "src/repl")
   (:file "src/sandbox")
   (:file "src/init")))
