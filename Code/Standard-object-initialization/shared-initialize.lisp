(cl:in-package #:clostrophilia)

;;; We assume this code is executed to create the generic function
;;; SHARED-INITIALIZE and its method with refinement R.  Therefore,
;;; the INSTANCE argument has refinement R+1.  CLASS-OF is called with
;;; INSTANCE as its argument, so CLASS-OF has refinement R just like
;;; SHARED-INITIALIZE, and CLASS-OF returns a class metaobject C of
;;; refinement R since the class of an instance of refinement R+1 is
;;; an object of refinement R.  However, SHARED-INITIALIZE does most
;;; of its processing by accessing the slot metaobjects of C.  The
;;; slot metaobjects of a class of refinement R also have refinement
;;; R, so the functions that operate on those class metaobjects have
;;; refinement R-1.  And the function CLASS-SLOTS that accesses the
;;; slot metaobjects of C also has refinement R-1.
;;;
;;; We thus have a choice.  We can either call versions R-1 of the
;;; functions that operate on C and its slot metaobjects directly from
;;; here.  Or we can call version R-1 of an auxiliary function that
;;; handles the part of SHARED-INITIALIZE that needs to access C and
;;; its slot metaobjects.  We choose the second solution.

(defgeneric shared-initialize
    (object slot-names &rest initargs &key &allow-other-keys))

(defmethod shared-initialize
    ((instance standard-object) slot-names &rest initargs)
  (apply #'shared-initialize-aux-1
         instance (class-of instance) slot-names initargs)
  instance)

; LocalWords:  metaobjects metaobject
