(cl:in-package #:clostrophilia)

(defun slot-boundp-using-only-class (class object slot-name)
  (let ((slot (find-slot class object slot-name 'slot-boundp)))
    (slot-boundp-using-class class object slot)))

(defun slot-value-using-only-class (class object slot-name)
  (let ((slot (find-slot class object slot-name 'slot-value)))
    (slot-value-using-class class object slot)))

(defun (setf slot-value-using-only-class) (new-value class object slot-name)
  (let ((slot (find-slot class object slot-name '(setf slot-value))))
    (setf (slot-value-using-class class object slot) new-value)))

(defun slot-makunbound-using-only-class (class object slot-name)
  (let ((slot (find-slot class object slot-name 'slot-makunbound)))
    (slot-makunbound-using-class class object slot)))
