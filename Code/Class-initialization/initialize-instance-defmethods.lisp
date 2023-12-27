(cl:in-package #:clostrophilia)

;;; The MOP specification says that the :DIRECT-DEFAULT-INITARGS
;;; keyword argument must be a proper list of canonicalized default
;;; initialization arguments, and that when a class is initialized, it
;;; defaults to the empty list.
;;;
;;; The MOP specification also says that when a class is initialized,
;;; then if the :DIRECT-SUPERCLASSES keyword argument is either not
;;; given or it is the empty list, then it default depending on the
;;; class being initialized.  We do that by calling another generic
;;; function DEFAULT-SUPERCLASSES with CLASS as its argument.

(defmethod initialize-instance :around
    ((class class)
     &rest initargs
     &key
       (direct-default-initargs '())
       (direct-slots '())
       (direct-superclasses '())
       (documentation nil)
     &allow-other-keys)
  (check-direct-default-initargs direct-default-initargs)
  (check-superclass-list class direct-superclasses)
  (let ((defaulted-direct-superclasses
          (if (null direct-superclasses)
              (default-superclasses class)
              direct-superclasses))
        (direct-slot-definitions
          (check-and-convert-direct-slot-specifications class direct-slots)))
    (check-documentation documentation)
    (loop for defaulted-direct-superclass in defaulted-direct-superclasses
          do (add-direct-subclass defaulted-direct-superclass class))
    (loop for direct-slot-definition in direct-slot-definitions
          do (create-readers-and-writers class direct-slot-definition))
    (apply #'call-next-method
           class
           ;; We supply the DIRECT-DEFAULT-INITARGS argument
           ;; explicitly, in case it was not given.  That way it
           ;; becomes defaulted to the empty list in that case.
           :direct-default-initargs direct-default-initargs
           ;; We supply the DIRECT-SLOTS argument explicitly, in case
           ;; it was not given.  That way it becomes defaulted to the
           ;; empty list in that case.
           :direct-slots direct-slot-definitions
           ;; We supply the DOCUMENTATION argument explicitly, in case
           ;; it was not given.  That way it becomes defaulted to the
           ;; empty list in that case.
           :documentation documentation
           :direct-superclasses defaulted-direct-superclasses
           initargs)))
