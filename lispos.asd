;;;; lispos.asd - ASDF System Definition for LispOS Monolithic Userland

(asdf:defsystem :lispos
  :description "Monolithic Common Lisp Linux Userland (LispOS)"
  :author "LispOS Team"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cffi :slynk :usocket :sb-posix)
  :serial t
  :components
  ((:file "src/package")
   (:file "src/paths")
   (:file "src/syscalls")
   (:file "src/drm")
   (:file "src/input")
   (:file "src/repl")
   (:file "src/sandbox")
   (:file "src/init")))
