(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-boundp-using-class.html
(defgeneric slot-boundp-using-class (class object slot))

(defun slot-boundp-using-class-default (class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (not (eq (car location) +unbound-slot-value+))
        (slot-boundp-using-location+1 object location))))

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
