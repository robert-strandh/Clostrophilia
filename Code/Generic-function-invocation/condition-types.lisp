(cl:in-package #:clostrophilia)

(define-condition no-applicable-method-error (error)
  ((%function
    :initarg :function
    :reader no-applicable-method-error-function)
   (%arguments
    :initarg :arguments :reader arguments))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to invoke the following generic function:~@
                      ~s
                      on the following arguments:~@
                      ~s
                      but the function has no method that is applicalble~@
                      to those arguments."
                     (no-applicable-method-error-function condition)
                     (arguments condition)))))
