(cl:in-package #:clostrophilia)

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
