(cl:in-package #:clostrophilia)

(defun slot-boundp-using-location (object location)
  (not (eq (standard-instance-access object location)
           +unbound-slot-value+))

;;; This function is called only when it is known that the slot is
;;; bound.
(defun slot-value-using-location (object location)
  (standard-instance-access object location))

(defun (setf slot-value-using-location) (value object location)
  (setf (standard-instance-access object location)
        value))

(defun slot-makunbound-using-location (object location)
  (setf (standard-instance-access object location)
        +unbound-slot-value+))
  
