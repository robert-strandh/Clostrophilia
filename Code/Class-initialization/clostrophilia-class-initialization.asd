(cl:in-package #:asdf-user)

;;;; The MOP specification states how each argument to MAKE-INSTANCE
;;;; should be handled when the object being created is a metaobject.
;;;; It states that, unless explicitly mentioned, when a metaobject is
;;;; reinitialized and no defaulting behavior is specified for the
;;;; argument, then if it is not explicitly given, it should not be
;;;; defaulted.
;;;;
;;;; These are the cases for the defaulting behavior of the argument
;;;; if it is not supplied:
;;;;
;;;;   1. It should be supplied neither at initialization nor at
;;;;      reinitialization.  This case does not result in any code.
;;;;
;;;;   2. It should be defaulted at initialization and not during
;;;;      reinitialization.  To handle this case, we supply the
;;;;      default value as the initialization argument in an :AROUND
;;;;      method on INITIALIZE-INSTANCE.  We always supply the value
;;;;      of the argument explicitly to CALL-NEXT-METHOD.
;;;;
;;;;   3. It should be defaulted both at initialization and
;;;;      reinitialization.  To handle this case, we supply the
;;;;      default value as the initialization argument in an :AROUND
;;;;      method on SHARED-INITIALIZE.  We always supply the value of
;;;;      the argument explicitly to CALL-NEXT-METHOD.
;;;;
;;;; For an argument that IS supplied, there are the following
;;;; possibilities according to whether the value needs to be checked
;;;; or not.
;;;;
;;;;   a. Its value does not have to be checked.  This case does not
;;;;      result in any code.
;;;;
;;;;   b. Its value has to be checked.  To handle this case we check
;;;;      the value in a :BEFORE method on SHARED-INITIALIZE.
;;;;
;;;; For an argument that IS supplied, there are the following
;;;; possibilities according to whether the value needs to be
;;;; transformed or not.
;;;;
;;;;    I.   Its value does not need to be transformed.  This case does
;;;;         not result in any code
;;;;
;;;;    II.  Its value needs to be transformed, but either only during
;;;;         initialization, or differently during initialization and
;;;;         during reinitialization.  The code for the transformation
;;;;         during initialization is in an :AROUND method on
;;;;         INITIALIZE-INSTANCE.  If transformation is needed during
;;;;         reinitialization then the code for that transformation is
;;;;         in an :AROUND method on REINITIALIZE-INSTANCE.
;;;;
;;;;    III. Its value needs to be transformed in the same way during
;;;;         initialization and reinitialization.  The code for
;;;;         transformation is in an :AROUND method on
;;;;         SHARED-INSTANCE.
;;;;
;;;; For class initialization, we have the following possible
;;;; arguments:
;;;;
;;;;   :DIRECT-DEFAULT-INITARGS is in category 2.b.I
;;;;
;;;;   :DIRECT-SLOTS is in category 2.b.III
;;;; 
;;;;   :DIRECT-SUPERCLASSES is in category 2.b.II
;;;; 
;;;;   :DOCUMENTATION is in category 2.b.I
;;;; 
;;;;   :NAME is in category 2.a.I

(defsystem "clostrophilia-class-initialization"
  :depends-on ("ecclesia")
  :serial t
  :components
  ((:file "remove-direct-subclass-defgeneric")
   (:file "remove-direct-subclass-defmethod")
   (:file "add-direct-subclass-defgeneric")
   (:file "add-direct-subclass-defmethod")
   (:file "default-superclasses")
   (:file "direct-slot-definition-class")
   (:file "reader-method-class")
   (:file "writer-method-class")
   (:file "validate-superclass-defgeneric")
   (:file "validate-superclass-defmethods")
   (:file "check-direct-default-initargs-defun")
   (:file "check-superclass-list-defun")
   (:file "add-accessor-method")
   (:file "special-variables")
   (:file "initialize-instance-defmethods")
   (:file "reinitialize-instance-defmethods")
   (:file "shared-initialize-methods")
   (:file "allocate-instance")
   (:file "allocate-instance-using-class")
   (:file "condition-types")))

; LocalWords:  reinitialization
