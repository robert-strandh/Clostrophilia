(cl:in-package #:clostrophilia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FINALIZE-INHERITANCE.
;;;
;;; The AMOP says that class finalization is done in three steps:
;;;
;;;   1. Compute the class precedence list and associate it with the
;;;      class so that it is returned by CLASS-PRECEDENCE-LIST.
;;;
;;;   2. Compute the effective slots and associate them with the
;;;      class, so that they are returned by CLASS-SLOTS.
;;;
;;;   3. Compute the default initargs and associate them with the
;;;      class, so that they are returned by CLASS-DEFAULT-INIARGS.
;;;
;;; The problem with this scenario is that steps 2 and 3 would call
;;; CLASS-PRECEDENCE-LIST to access the class precedence list computed
;;; in step 1.  However, the AMOP also specifies that the generic
;;; function CLASS-PRECEDENCE-LIST signals an error if the class is
;;; not finalized, which it will not be until step 3 is completed!
;;;
;;; The only solution we can see is for steps 2 and 3 to access the
;;; class precedence list using way other than calling the function
;;; CLASS-PRECEDENCE-LIST; either with a different reader, or by using
;;; SLOT-VALUE.  SLOT-VALUE is unappealing because it might be slow,
;;; so we choose the first solution, and define an ACCESSOR called
;;; PRECEDENCE-LIST.
;;;
;;; The AMOP says that ALLOCATE-INSTANCE calls CLASS-FINALIZED-P to
;;; check whether the class is finalized, and if it is not, then it
;;; calls FINALIZE-INHERITANCE.  We use this information to define
;;; FINALIZE-INHERITANCE to always finalize the class, independently
;;; of whether it is already finalized.  In the worst case, some
;;; function other than ALLOCATE-INSTANCE might forget to test whether
;;; the class is finalized before calling FINALIZE-INHERITANCE in
;;; which case nothing bad happens other than a performance
;;; degradation.
;;;
;;; The AMOP does not tell use whether the fact that a class is
;;; finalized implies that all its superclasses are finalized as well,
;;; and it MIGHT make sense to allow that.  On the other hand, all the
;;; steps involved in finalizing a class require basically the same
;;; processing in the superclasses, so we do not allow this, and
;;; instead require all the superclasses of a finalized class to be
;;; finalized as well.

;;; We define this function as a separate abstraction, because during
;;; bootstrapping, it might not necessarily be desirable to call
;;; CLASS-FINALIZED-P, and during bootstrapping, we have full control
;;; over class finalization, so we can make sure it is done so that it
;;; can be assumed that all the superclasses of some class C are all
;;; finalized before an attempt is made to finalize C itself.
(defun finalize-inheritance-assuming-superclasses-finalized (class)
  ;; While we can assume superclasses to be finalized, we can't call
  ;; COMPUTE-CLASS-PRECEDENCE-LIST-ASSUMING-SUPERCLASSES-FINALIZED
  ;; because COMPUTE-CLASS-PRECEDENCE-LIST may have overriding methods
  ;; on it that we are not aware of here.
  (setf (precedence-list class) (compute-class-precedence-list class))
  (let* ((effective-slots (compute-slots class))
         (slot-count
           (count :instance effective-slots
                  :test #'eq :key #'slot-definition-allocation)))
    (setf (instance-size class) slot-count)
    (setf (class-slots class) effective-slots))
  (setf (class-default-initargs class) (compute-default-initargs class))
  (setf (unique-number class) (new-unique-number))
  ;; We set FINALIZED-P to TRUE before allocating the prototype
  ;; instance, because ALLOCATE-INSTANCE checks that the class is
  ;; finalized and if not, signals an error.
  (setf (class-finalized-p class) t)
  (setf (class-prototype class)
        (allocate-instance class)))

(defmethod finalize-inheritance ((class real-class))
  ;; Make sure all the direct superclasses are already finalized.
  ;; Since FINALIZE-INHERITANCE does not itself test whether the class
  ;; is finalized before finalizing it, we do it in this loop so as to
  ;; avoid calling FINALIZE-INHERITANCE unnecessarily.
  (loop for super in (class-direct-superclasses class)
        do (unless (class-finalized-p super)
             (finalize-inheritance super)))
  (finalize-inheritance-assuming-superclasses-finalized class))
