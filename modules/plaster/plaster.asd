
(in-package #:cl-user)

(asdf:defsystem #:plaster
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :version "1.0.0"
  :components ((:file "module")
               (:file "frontend"))
  :depends-on (:r-clip))

