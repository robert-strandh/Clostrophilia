(cl:in-package #:asdf-user)

(defsystem clostrophilia-generic-function-invocation
  :serial t
  :components
  ((:file "classp")
   (:file "sub-specializer-p")
   (:file "compute-applicable-methods-using-classes")
   (:file "compute-applicable-methods")
   (:file "compute-effective-method")))
