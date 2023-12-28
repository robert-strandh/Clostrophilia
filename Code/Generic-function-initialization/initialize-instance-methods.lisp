(cl:in-package #:clostrophilia)

(defmethod initialize-instance :around
    ((generic-function generic-function)
     &rest initargs
     &key
       (declarations '())
       (documentation nil)
     &allow-other-keys)
  (check-generic-function-declarations declarations)
  (check-documentation documentation))
