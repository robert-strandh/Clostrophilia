(cl:in-package #:clostrophilia)

(defmethod initialize-instance :before
    ((generic-function generic-function)
     &key
       method-combination
     &allow-other-keys)
  (unless (method-combination-p method-combination)
    (error 'method-combination-option-must-be-method-combination
           :method-combination method-combination)))

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
  (unless (subtypep-1 method-class (find-class 'method))
    (error 'method-class-option-must-be-method-class
           :method-class method-class))
  (apply #'call-next-method
         generic-function
         :name name
         :declarations declarations
         :documentation documentation
         :method-class method-class
         initargs))
