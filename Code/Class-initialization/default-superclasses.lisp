(cl:in-package #:clostrophilia)

(defvar *standard-object*)

(defvar *funcallable-standard-object*)

(defgeneric default-superclasses (class))

(defmethod default-superclasses (class)
  '())

(defmethod default-superclasses ((class standard-class))
  (list *standard-object*))

(defmethod default-superclasses ((class funcallable-standard-class))
  (list *funcallable-standard-object*))
