(cl:in-package #:clostrophilia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MISSING.

(defmethod slot-missing
    (class object slot-name operation &optional new-value)
  (declare (ignore class operation new-value))
  (error 'slot-missing
         :name slot-name
         :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-UNBOUND.

(defmethod slot-unbound (class object slot-name)
  (declare (ignore class))
  (error 'unbound-slot
         :name slot-name
         :instance object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-VALUE, (SETF SLOT-VALUE),
;;; SLOT-VALUE-USING-CLASS (SETF SLOT-VALUE-USING-CLASS)

(defun slot-value-using-class-default (class object slot)
  (let* ((location (slot-definition-location slot))
         (value
           (if (consp location)
               (car location)
               (standard-instance-access object location))))
    (if (eq value +unbound-slot-value+)
        (slot-unbound class object (slot-definition-name slot))
        value)))

(defmethod slot-value-using-class
    ((class standard-class)
     object
     (slot standard-effective-slot-definition))
  (slot-value-using-class-default class object slot))

(defmethod slot-value-using-class
    ((class funcallable-standard-class)
     object
     (slot standard-effective-slot-definition))
  (slot-value-using-class-default class object slot))

(defmethod slot-value-using-class
    ((class built-in-class)
     object
     slot)
  (declare (ignorable class) (ignore object slot))
  (error 'no-slots-in-a-built-in-class
         :object object))

(defun (setf slot-value-using-class-default) (new-value class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (setf (car location) new-value)
        (setf (standard-instance-access object location) new-value))))

(defmethod (setf slot-value-using-class)
    (new-value
     (class standard-class)
     object
   (slot standard-effective-slot-definition))
  (setf (slot-value-using-class-default class object slot) new-value))

(defmethod (setf slot-value-using-class)
  (new-value
   (class funcallable-standard-class)
   object
   (slot standard-effective-slot-definition))
  (setf (slot-value-using-class-default class object slot) new-value))

(defmethod (setf slot-value-using-class)
  (new-value
   (class built-in-class)
   object
   slot)
  (declare (ignorable class) (ignore new-value object slot))
  (error 'no-slots-in-a-built-in-class
         :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-BOUNDP-USING-CLASS

(defun slot-boundp-using-class-default (class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (not (eq (car location) +unbound-slot-value+))
        (slot-boundp-using-index object location))))

(defmethod slot-boundp-using-class
    ((class standard-class)
     object
     (slot standard-effective-slot-definition))
  (slot-boundp-using-class-default class object slot))

(defmethod slot-boundp-using-class
    ((class funcallable-standard-class)
     object
     (slot standard-effective-slot-definition))
  (slot-boundp-using-class-default class object slot))

(defmethod slot-boundp-using-class
    ((class built-in-class)
     object
     slot)
  (declare (ignorable class) (ignore object slot))
  (error 'no-slots-in-a-built-in-class
         :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MAKUNBOUND, SLOT-MAKUNBOUND-USING-CLASS.

(defun slot-makunbound-using-class-default (class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (setf (car location) +unbound-slot-value+)
        (slot-makunbound-using-index object location)))
  nil)

(defmethod slot-makunbound-using-class
  ((class standard-class)
   object
   (slot standard-effective-slot-definition))
  (slot-makunbound-using-class-default class object slot))

(defmethod slot-makunbound-using-class
  ((class funcallable-standard-class)
   object
   (slot standard-effective-slot-definition))
  (slot-makunbound-using-class-default class object slot))

(defmethod slot-makunbound-using-class
  ((class built-in-class)
   object
   slot)
  (declare (ignorable class) (ignore object slot))
  (error 'no-slots-in-a-built-in-class
         :object object))
