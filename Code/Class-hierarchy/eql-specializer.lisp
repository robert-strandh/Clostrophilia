(cl:in-package #:clostrophilia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class EQL-SPECIALIZER.

(defgeneric eql-specializer-object (eql-specializer))

(defclass eql-specializer (specializer)
  ((%object 
    :initarg :object 
    :reader eql-specializer-object)))
