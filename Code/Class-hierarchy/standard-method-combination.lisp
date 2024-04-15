(cl:in-package #:clostrophilia)

(defgeneric template (method-combination))

(defgeneric variant-signature (method-combination))

(defclass standard-method-combination (method-combination)
  ((%template :initarg :template :reader template)
   (%variant-signature :initarg :variant-signature :reader variant-signature)))
