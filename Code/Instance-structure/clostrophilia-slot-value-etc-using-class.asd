(cl:in-package asdf-user)

(defsystem "clostrophilia-slot-value-etc-using-class"
  :serial t
  :components
  ((:file "slot-missing")
   (:file "slot-unbound")
   (:file "slot-boundp-using-class")
   (:file "slot-value-using-class")
   (:file "slot-makunbound-using-class")
   (:file "find-slot")
   (:file "slot-value-etc-using-only-class")
   (:file "condition-types")))
