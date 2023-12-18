(cl:in-package #:asdf-user)

(defsystem "clostrophilia-class-initialization"
  :serial t
  :components
  ((:file "remove-direct-subclass-defgeneric")
   (:file "remove-direct-subclass-defmethod")
   (:file "add-direct-subclass-defgeneric")
   (:file "add-direct-subclass-defmethod")
   (:file "initialize-instance-defmethods")))
