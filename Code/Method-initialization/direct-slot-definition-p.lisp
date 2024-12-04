(cl:in-package #:clostrophilia)

(defgeneric direct-slot-definition-p (object))

(defmethod direct-slot-definition-p (object)
  nil)

(defmethod direct-slot-definition-p ((object direct-slot-definition))
  t)
