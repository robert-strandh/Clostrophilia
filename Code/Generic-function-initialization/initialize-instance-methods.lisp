(cl:in-package #:clostrophilia)

(defmethod initialize-instance :before
    ((generic-function generic-function)
     &rest initargs
     &key
       (lambda-list nil lambda-list-p)
       (argument-precedence-order nil argument-precedence-order-p)
     &allow-other-keys)
  (when (and argument-precedence-order-p (not lambda-list-p))
    (error 'argument-precedence-order-given-but-not-lambda-list
           :argument-precedence-order argument-precedence-order )))
