(cl:in-package #:clostrophilia)

(defgeneric default-superclasses (class))

(defmethod default-superclasses (class)
  '())

(defmethod default-superclasses ((class standard-class))
  (list (find-class-standard-object)))

(defmethod default-superclasses ((class funcallable-standard-class))
  (list (find-class-funcallable-standard-object)))
