(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-makunbound-using-class.html
(defgeneric slot-makunbound-using-class (class object slot))

(defun slot-makunbound-using-class-default (class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (setf (car location) +unbound-slot-value+)
        (slot-makunbound-using-location+1 object location)))
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
