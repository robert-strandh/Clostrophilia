(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-name.html
(defgeneric class-name (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-subclasses.html
(defgeneric class-direct-subclasses (class))

;;; The functions ADD-DIRECT-SUBCLASS and REMOVE-DIRECT-SUBCLASS are
;;; used to update the direct subclasses of a class, so they call this
;;; function.
(defgeneric (setf class-direct-subclasses) (direct-subclassees class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-superclasses.html
;;;
;;; The slot corresponding to this generic function has different
;;; slot options in different subclasses of the class CLASS, which
;;; is why the slot itself is not included here.
(defgeneric class-direct-superclasses (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-slots.html
;;;
;;; The slot corresponding to this generic function has different
;;; slot options in different subclasses of the class CLASS, which
;;; is why the slot itself is not included here.
(defgeneric class-direct-slots (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-default-initargs.html
;;;
;;; The slot corresponding to this generic function has different
;;; slot options in different subclasses of the class CLASS, which
;;; is why the slot itself is not included here.
(defgeneric class-direct-default-initargs (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-finalized-p.html
;;;
;;; The slot corresponding to this generic function has different
;;; slot options in different subclasses of the class CLASS, which
;;; is why the slot itself is not included here.
(defgeneric class-finalized-p (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-default-initargs.html
(defgeneric class-default-initargs (class))

;;; This function is used by the class finalization protocol to set
;;; the default initargs of the class. 
(defgeneric (setf class-default-initargs) (default-initargs class))

(defclass class (specializer)
  ((%name 
    :initform nil
    :initarg :name 
    ;; There is a specified function named (SETF CLASS-NAME), but it
    ;; is not an accessor.  Instead it works by calling
    ;; REINITIALIZE-INSTANCE with the new name.
    :reader class-name)
   (%direct-subclasses 
    :initform '() 
    :initarg :direct-subclasses
    :accessor class-direct-subclasses)))
