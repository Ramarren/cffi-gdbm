(asdf:defsystem cffi-gdbm
  :version "0"
  :description "CFFI bindings for GDBM"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "GPL"
  :depends-on (:cffi :alexandria :babel)
  :components ((:file "package")
               (:file "bindings" :depends-on ("package"))
               (:file "datum" :depends-on ("package" "bindings"))
               (:file "interface" :depends-on ("package" "bindings" "datum"))))