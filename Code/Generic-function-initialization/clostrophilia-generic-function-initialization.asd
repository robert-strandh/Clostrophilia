(cl:in-package #:asdf-user)

(defsystem "clostrophilia-generic-function-initialization"
  :serial t
  :components
  ((:file "initialize-instance-methods")
   (:file "condition-types")))
