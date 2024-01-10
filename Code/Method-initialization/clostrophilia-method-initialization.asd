(cl:in-package #:asdf-user)

(defsystem "clostrophilica-method-initialization"
  :serial t
  :components
  ((:file "condition-types")
   (:file "initialize-instance-methods")
   (:file "reinitialize-instance-methods")))
