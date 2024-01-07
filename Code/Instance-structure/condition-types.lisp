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

(define-condition unbound-slot (cell-error)
  ((%instance :initarg :instance :reader unbound-slot-instance))
  (:report (lambda (condition stream)
             (format stream
                     "The slot named:~@
                      ~s~@
                      is unbound in the object:~@
                      ~s"
                     (cell-error-name condition)
                     (unbound-slot-instance condition)))))
