(cl:in-package #:clostrophilia)

(defmethod reinitialize-instance :around
    ((class class)
     &rest initargs
     &key
       (direct-superclasses '() direct-superclasses-p)
     &allow-other-keys)
  (let ((*existing-superclasses* (class-direct-subclasses class)))
    (if direct-superclasses-p
        (if (null direct-superclasses)
            (apply #'call-next-method
                   :direct-superclasses (default-superclasses class)
                   initargs)
            (call-next-method))
        (call-next-method))))
