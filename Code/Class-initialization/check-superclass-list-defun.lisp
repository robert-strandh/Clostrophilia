(cl:in-package #:clostrophilia)

(defun check-superclass-list (class superclasses)
  (unless (ecclesia:proper-list-p superclasses)
    (error 'superclasses-must-be-proper-list
           :superclasses superclasses))
  (loop for superclass in superclasses
        do (unless (classp superclass)
             (error 'type-error
                    :datum superclass
                    :expected-type 'class))
           (unless (validate-superclass class superclass)
             (error 'class-can-not-be-superclass
                    :subclass class
                    :superclass superclass))))
