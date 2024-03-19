(cl:in-package asdf-user)

(defsystem "clostrophilia-instance-structure"
  :serial t
  :components
  ((:file "generic-functions")
   (:file "slot-bound-using-index")
   (:file "slot-missing")
   (:file "slot-unbound")
   (:file "slot-boundp-using-class")
   (:file "slot-value-using-class")
   (:file "slot-makunbound-using-class")
   (:file "methods")
   (:file "condition-types")))
