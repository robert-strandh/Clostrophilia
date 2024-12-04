(cl:in-package #:asdf-user)

(defsystem "clostrophilia-method-initialization"
  :serial t
  :components
  ((:file "condition-types")
   (:file "direct-slot-definition-p")
   (:file "initialize-instance-methods")
   (:file "reinitialize-instance-methods")))
