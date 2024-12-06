(cl:in-package #:clostrophilia)

(defgeneric method-combination-p (object))

(defmethod method-combination-p (object)
  nil)

(defmethod method-combination-p ((object method-combination))
  t)
