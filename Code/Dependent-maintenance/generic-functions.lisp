(cl:in-package #:clostrophilia)

(defgeneric add-dependent (metaobject dependent))

(defgeneric remove-dependent (metaobject dependent))

(defgeneric map-dependents (metaobject function))

(defgeneric update-dependent (metaobject dependent &rest initargs))
