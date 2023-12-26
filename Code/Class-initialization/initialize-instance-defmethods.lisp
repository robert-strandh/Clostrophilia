(cl:in-package #:clostrophilia)

;;; The MOP specification also says that the :DIRECT-DEFAULT-INITARGS
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
    ((class class) &rest initargs
     &key
       (direct-default-initargs '())
       (direct-superclasses '())
     &allow-other-keys)
  (check-direct-default-initargs direct-default-initargs)
  (check-superclass-list class direct-superclasses)
  (let ((defaulted-direct-superclasses
          (if (null direct-superclasses)
              (default-superclasses class)
              direct-superclasses)))
    (loop for defaulted-direct-superclass in defaulted-direct-superclasses
          do (add-direct-subclass defaulted-direct-superclass class))
    (apply #'call-next-method
           class
           ;; We supply the DIRECT-DEFAULT-INITARGS argument
           ;; explicitly, in case it was not given.  That way it
           ;; becomes defaulted to the empty list in that case.
           :direct-default-initargs direct-default-initargs
           :direct-superclasses defaulted-direct-superclasses
           initargs)))
