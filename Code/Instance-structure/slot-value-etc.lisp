(cl:in-package #:clostrophilia)

(defun slot-boundp (object slot-name)
  (slot-boundp-using-only-class (class-of object) object slot-name))

(defun slot-value (object slot-name)
  (slot-value-using-only-class (class-of object) object slot-name))

(defun (setf slot-value) (value object slot-name)
  (setf (slot-value-using-only-class (class-of object) object slot-name)
        value))

(defun slot-makunbound (object slot-name)
  (slot-makunbound-using-only-class (class-of object) object slot-name))
