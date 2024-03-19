(cl:in-package asdf-user)

(defsystem "clostrophilia-instance-structure"
  :serial t
  :components
  ((:file "generic-functions")
   (:file "slot-bound-using-index")
   (:file "slot-missing")
   (:file "slot-unbound")
   (:file "methods")
   (:file "condition-types")))
