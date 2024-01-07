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
  (check-documentation documentation)
  (unless (typep method-class 'method)
    (error 'method-class-option-must-be-method-class
           :method-class method-class))
  (apply #'call-next-method
         name
         declarations
         documentation
         method-class
         initargs))

