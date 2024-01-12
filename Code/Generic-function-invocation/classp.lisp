(cl:in-package #:clostrophilia)

;;; Return true if and only if OBJECT is a class.
(defgeneric classp (object))

(defmethod classp (object)
  nil)

(defmethod classp ((object class))
  t)
