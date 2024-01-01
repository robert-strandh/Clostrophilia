(cl:in-package #:clostrophilia)

(defgeneric writer-method-class
    (class direct-slot-definition &rest initargs))

(defmethod writer-method-class
    ((class standard-class)
     (direct-slot-definition standard-direct-slot-definition)
     &rest initargs)
  (find-class 'standard-writer-method))

(defmethod writer-method-class
    ((class funcallable-standard-class)
     (direct-slot-definition standard-direct-slot-definition)
     &rest initargs)
  (find-class 'standard-writer-method))
