(cl:in-package asdf-user)

(defsystem "clostrophilia-instance-structure"
  :serial t
  :components
  ((:file "generic-functions")
   (:file "methods")
   (:file "condition-types")))
