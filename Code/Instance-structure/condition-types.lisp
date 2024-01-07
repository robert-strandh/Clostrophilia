(cl:in-package #:clostrophilia)

(define-condition slot-missing (error)
  ((%name :initarg :name :reader name)
   (%object :initarg :object :reader object))
  (:report (lambda (condition stream)
             (format stream
                     "The slot named:~@
                      ~s~@
                      is missing from the object:~@
                      ~s"
                     (name condition)
                     (object condition)))))
