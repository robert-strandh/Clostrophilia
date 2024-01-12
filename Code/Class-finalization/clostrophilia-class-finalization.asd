(cl:in-package #:asdf-user)

(defsystem clostrophilia-class-finalization
  :serial t
  :components
  ((:file "generic-functions")
   (:file "compute-class-precedence-list")
   (:file "compute-default-initargs")
   (:file "effective-slot-definition-class")
   (:file "compute-effective-slots")
   (:file "finalize-inheritance")))
