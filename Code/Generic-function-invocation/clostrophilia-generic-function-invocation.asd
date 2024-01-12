(cl:in-package #:asdf-user)

(defsystem clostrophilia-generic-function-invocation
  :serial t
  :components
  ((:file "sub-specializer-p")
   (:file "compute-applicable-methods-using-classes")))
