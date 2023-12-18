(cl:in-package #:clostrophilia)

;;;; The MOP specification includes a single method on this generic
;;;; function, specialized for CLASS and CLASS.  The specification
;;;; says that no error is signaled when SUBCLASS is not a member of
;;;; the direct subclasses of SUPERCLASS.

(defmethod remove-direct-subclass ((superclass class) (subclass class))
  (setf (class-direct-subclasses superclass)
        (remove subclass (class-direct-subclasses superclass)
                :test #'eq)))
