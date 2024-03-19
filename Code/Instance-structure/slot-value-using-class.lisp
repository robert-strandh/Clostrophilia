(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-value-using-class.html
(defgeneric slot-value-using-class (class object slot))

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

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-slot-value-using-class.html
(defgeneric (setf slot-value-using-class) (new-value class object slot))

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
