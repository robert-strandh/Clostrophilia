(cl:in-package #:clostrophilia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-OBJECT.

(defgeneric hash-code (standard-object))

(defclass standard-object (t)
  ((%hash :initform (random #.(ash 1 62))
          :reader hash-code)))
