(cl:in-package #:clostrophilia)

(defmethod add-dependent ((class standard-class) dependent)
  (pushnew dependent (dependents class) :test #'eq))

(defmethod add-dependent ((class funcallable-standard-class) dependent)
  (pushnew dependent (dependents class) :test #'eq))

(defmethod add-dependent
    ((generic-function standard-generic-function) dependent)
  (pushnew dependent (dependents generic-function) :test #'eq))

(defmethod remove-dependent ((class standard-class) dependent)
  (setf (dependents class)
        (remove dependent (dependents class) :test #'eq)))

(defmethod remove-dependent ((class funcallable-standard-class) dependent)
  (setf (dependents class)
        (remove dependent (dependents class) :test #'eq)))

(defmethod remove-dependent
    ((generic-function standard-generic-function) dependent)
  (setf (dependents generic-function)
        (remove dependent (dependents generic-function) :test #'eq)))

(defmethod map-dependents ((class standard-class) function)
  (mapc function (dependents class)))

(defmethod map-dependents ((class funcallable-standard-class) function)
  (mapc function (dependents class)))

(defmethod map-dependents
    ((generic-function standard-generic-function) function)
  (mapc function (dependents generic-function)))
