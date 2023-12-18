(cl:in-package #:clostrophilia)

;;; The AMOP says that when a class is initialized, then if the
;;; :DIRECT-SUPERCLASSES keyword argument is either not given or it is
;;; the empty list, then it default depending on the class being
;;; initialized.  We do that by calling another generic function
;;; DEFAULT-SUPERCLASSES with CLASS as its argument.

(defmethod initialize-instance :around
    ((class class) &rest initargs &key direct-superclasses &allow-other-keys)
  (let ((defaulted-direct-superclasses
          (if (null direct-superclasses)
              (default-superclasses class)
              direct-superclasses)))
    (loop for defaulted-direct-superclass in defaulted-direct-superclasses
          do (add-direct-subclass defaulted-direct-superclass class))
    (apply #'call-next-method
           class
           :direct-superclasses defaulted-direct-superclasses
           initargs)))
