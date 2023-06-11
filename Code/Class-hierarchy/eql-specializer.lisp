(cl:in-package #:clostrophilia)

(defgeneric eql-specializer-object (eql-specializer))

(defclass eql-specializer (specializer)
  ((%object 
    :initarg :object 
    :reader eql-specializer-object)))
