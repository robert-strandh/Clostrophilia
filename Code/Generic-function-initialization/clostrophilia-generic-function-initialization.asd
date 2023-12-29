(cl:in-package #:asdf-user)

(defsystem "clostrophilia-generic-function-initialization"
  :serial t
  :components
  ((:file "check-declarations")
   (:file "check-documentation")
   (:file "initialize-instance-methods")
   (:file "reinitialize-instance-methods")
   (:file "shared-initialize-methods")
   (:file "condition-types")))
