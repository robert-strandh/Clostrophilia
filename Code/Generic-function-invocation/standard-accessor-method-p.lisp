(cl:in-package #:clostrophilia)

(defgeneric standard-accessor-method-p (object))

(defmethod standard-accessor-method-p (object)
  nil)

(defmethod standard-accessor-method-p ((object standard-accessor-method))
  t)
