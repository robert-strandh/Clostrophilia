(cl:in-package #:asdf-user)

(defsystem clostrophilia-slot-definition-initialization
  :serial t
  :components
  ((:file "condition-types")
   (:file "initialize-instance-methods")
   (:file "reinitialize-instance-methods")))
