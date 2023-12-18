(cl:in-package #:claustrophilia)

;;; The MOP specification says that if :DIRECT-SUPERCLASSES keyword
;;; argument is given when the class is reinitialized, then we must
;;; call REMOVE-DIRECT-SUBCLASS for every class in the difference
;;; between the existing direct superclasses and the new value, and
;;; then call ADD-DIRECT-SUBCLASS for every class in the difference
;;; between the new value and the existing direct superclasses.

(defmethod reinitialize-instance :before
    ((class class)
     &key (direct-superclasses nil direct-superclasses-supplied-p)
     &allow-other-keys)
  (when direct-superclasses-supplied-p
    (let* ((old (class-direct-superclasses class))
           (obsolete (set-difference old direct-superclasses))
           (new (set-difference direct-superclasses old)))
      (loop for direct-superclass in obsolete
            do (remove-direct-subclass direct-superclass class))
      (loop for direct-superclass in new
            do (add-direct-subclass direct-superclass class)))))
