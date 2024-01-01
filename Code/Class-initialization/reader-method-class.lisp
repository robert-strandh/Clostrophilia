(cl:in-package #:clostrophilia)

(defgeneric reader-method-class
    (class direct-slot-definition &rest initargs))

(defmethod reader-method-class
    ((class standard-class)
     (direct-slot-definition standard-direct-slot-definition)
     &rest initargs)
  (find-class 'standard-reader-method))

(defmethod reader-method-class
    ((class funcallable-standard-class)
     (direct-slot-definition standard-direct-slot-definition)
     &rest initargs)
  (find-class 'standard-reader-method))
