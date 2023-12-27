(cl:in-package #:clostrophilia)

;;; The MOP specification says that the :DIRECT-DEFAULT-INITARGS
;;; keyword argument, if given, must be a proper list of canonicalized
;;; default initialization arguments.  If it is not given during
;;; reinitialization, then it is not defaulted.
;;;
;;; The MOP specification says that if :DIRECT-SUPERCLASSES keyword
;;; argument is given when the class is reinitialized, then we must
;;; call REMOVE-DIRECT-SUBCLASS for every class in the difference
;;; between the existing direct superclasses and the new value, and
;;; then call ADD-DIRECT-SUBCLASS for every class in the difference
;;; between the new value and the existing direct superclasses.

;;; We define a :BEFORE method for arguments that only need to have
;;; their contents checked, but that should be passed unchanged to the
;;; primary method, or not passed if not given.
(defmethod reinitialize-instance :before
    ((class class)
     &key
       (direct-default-initargs '())
       (direct-superclasses '() direct-superclasses-supplied-p)
     &allow-other-keys)
  (check-direct-default-initargs direct-default-initargs)
  (check-superclass-list class direct-superclasses)
  (when direct-superclasses-supplied-p
    (let* ((old (class-direct-superclasses class))
           (obsolete (set-difference old direct-superclasses))
           (new (set-difference direct-superclasses old)))
      (loop for direct-superclass in obsolete
            do (remove-direct-subclass direct-superclass class))
      (loop for direct-superclass in new
            do (add-direct-subclass direct-superclass class)))))

;;; We define an :AROUND method for arguments that need to be
;;; transformed or defaulted before passed to the primary method.
;;;
;;; In case of :DIRECT-SLOTS, if it is given, it needs to be turned
;;; into a list of direct slot defininitions before being passed to
;;; the primary method.  If it is not given, it should not be passed
;;; at all to the primary method.
(defmethod reinitialize-instance :around
    ((class class)
     &rest initargs
     &key
       (direct-slots '() direct-slots-p)
     &allow-other-keys)
  (let ((direct-slot-definitions
          (check-and-convert-direct-slot-specifications class direct-slots)))
    (loop for direct-slot-definition in direct-slot-definitions
          do (create-readers-and-writers class direct-slot-definition))
    (if direct-slots-p
        (apply #'call-next-method class
               :direct-slots direct-slot-definitions
               initargs)
        (call-next-method))))
