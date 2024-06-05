(cl:in-package #:clostrophilia)

(defvar +unbound-slot-value+)

(defun slot-boundp-using-index (instance index)
  (not (eq (standard-instance-access instance index)
           +unbound-slot-value+)))

(defun slot-makunbound-using-index (instance index)
  (setf (standard-instance-access instance index)
        +unbound-slot-value+)
  nil)
