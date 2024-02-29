(cl:in-package #:clostrophilia)

(defmethod initialize-instance :around
    ((generic-function generic-function)
     &rest initargs
     &key
       (name nil)
       (declarations '())
       (documentation nil)
       (method-class (find-class 'standard-method))
     &allow-other-keys)
  (check-generic-function-declarations declarations)
  (check-generic-function-documentation documentation)
  (unless (subtypep method-class (find-class 'method))
    (error 'method-class-option-must-be-method-class
           :method-class method-class))
  (apply #'call-next-method
         generic-function
         :name name
         :declarations declarations
         :documentation documentation
         :method-class method-class
         initargs))

