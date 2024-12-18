(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/allocate-instance.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_alloca.htm#allocate-instance
(defgeneric allocate-instance (class &rest initargs))

;;; The specification includes three methods for this generic
;;; function.  The first one is specialized to STANDARD-CLASS.  The
;;; second one is specialized to FUNCALLABLE-STANDARD-CLASS.  The
;;; third one is specialized to BUILT-IN-CLASS.  The last one signals
;;; an error.

;;; The AMOP says that ALLOCATE-INSTANCE checks whether the class is
;;; finalized, and if not, calls FINALIZE-INHERITANCE.  However, the
;;; INITARGS received by ALLOCATE-INSTANCE should be the defaulted
;;; initargs, and computing the defaulted initargs requires the class
;;; to be finalized.  A peek at PCL shows that the class is finalized
;;; in MAKE-INSTANCE, before ALLOCATE-INSTANCE is called, which makes
;;; more sense.

;;; This method on ALLOCATE-INSTANCE is specialized to REGULAR-CLASS.
;;; Every instance of a regular class has two initial cells (the
;;; STAMP, and the list of effective slots of the class) in the rack.
;;; These cells are not counted among the slots, because they are
;;; accessed directly, using offsets.  For that reason, we must
;;; allocate more slot storage than there are slots with :INSTANCE
;;; allocation.

(defmethod allocate-instance
    ((class regular-class)
     &rest initargs
     &key (additional-size 0)
     &allow-other-keys)
  (let* ((size (+ (instance-size class) additional-size 2))
         (stamp (unique-number class))
         (slots (class-slots class))
         (instance (allocate-general-instance class size)))
    (setf (slot-value-using-location+1 instance -2) stamp
          (slot-value-using-location+1 instance -1) slots)
    instance))
