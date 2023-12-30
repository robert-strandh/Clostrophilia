(cl:in-package #:clostrophilia)


(defun convert-slot-specification-to-direct-slot-definition
    (class direct-slot-specification)
  (let ((direct-slot-definition-class
          (apply #'direct-slot-definition-class
                 class direct-slot-specification)))
    (apply #'make-instance direct-slot-definition-class
           direct-slot-specification)))

(defun check-and-convert-direct-slot-specifications
    (class direct-slot-specifications)
  (check-direct-slot-specifications direct-slot-specifications)
  (loop for direct-slot-specification in direct-slot-specifications
        collect (convert-slot-specification-to-direct-slot-definition
                 class direct-slot-specification)))
