(cl:in-package #:asdf-user)

(defsystem "clostrophilia-class-initialization"
  :depends-on ("ecclesia")
  :serial t
  :components
  ((:file "remove-direct-subclass-defgeneric")
   (:file "remove-direct-subclass-defmethod")
   (:file "add-direct-subclass-defgeneric")
   (:file "add-direct-subclass-defmethod")
   (:file "validate-superclass-defgeneric")
   (:file "validate-superclass-defmethods")
   (:file "check-superclass-list-defun")
   (:file "initialize-instance-defmethods")
   (:file "reinitialize-instance-defmethods")))
