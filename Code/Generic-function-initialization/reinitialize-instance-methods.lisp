(cl:in-package #:clostrophilia)

(defmethod reinitialize-instance :before
    ((generic-function generic-function)
     &key
       (method-class nil method-class-p)
     &allow-other-keys)
  (when method-class-p
    (unless (subtypep method-class (find-class 'method))
      (error 'method-class-option-must-be-method-class
             :method-class method-class))))
