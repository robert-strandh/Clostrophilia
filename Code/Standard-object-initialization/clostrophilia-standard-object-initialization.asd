(cl:in-package #:asdf-user)

(defsystem "clostrophilia-standard-object-initialization"
  :serial t
  :components
  ((:file "shared-initialize")
   (:file "initialize-instance")
   (:file "reinitialize-instance")))
