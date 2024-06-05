(cl:in-package #:clostrophilia)

(defun find-slot (class object slot-name operation
                  &optional (new-value nil new-value-p))
  (let* ((slots (class-slots class))
         (slot (find slot-name slots
                     :test #'eq :key #'slot-definition-name)))
    (if (null slot)
        (apply #'slot-missing class object slot-name operation
               (if (null new-value-p) '() (list new-value)))
        slot)))
