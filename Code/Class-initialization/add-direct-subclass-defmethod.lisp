(cl:in-package #:clostrophilia)

;;; The MOP specification includes a single method on this generic
;;; function, specialized for CLASS and CLASS.  The specification does
;;; not mention what happens if SUBCLASS is already a member of the
;;; list of direct subclasses of SUPERCLASS.  We choose to signal an
;;; error in this case.

(defmethod add-direct-subclass ((superclass class) (subclass class))
  (when (member subclass (class-direct-subclasses superclass))
    (error 'attempt-to-add-existing-subclass
           :subclass subclass
           :superclass superclass))
  (setf (class-direct-subclasses superclass)
        (cons subclass (class-direct-subclasses superclass))))
