(cl:in-package #:asdf-user)

(defsystem clostrophilia-generic-function-invocation
  :serial t
  :components
  ((:file "classp")
   (:file "sub-specializer-p")
   (:file "specializer-profile")
   (:file "compute-applicable-methods-using-classes")
   (:file "compute-applicable-methods")
   (:file "compute-effective-method")
   (:file "discriminating-automaton")
   (:file "discriminating-tagbody")
   (:file "maybe-replace-method")
   (:file "no-applicable-method")
   (:file "compute-discriminating-function")
   (:file "add-remove-direct-method")
   (:file "invalidate-discriminating-function")
   (:file "add-remove-method")
   (:file "condition-types")))
