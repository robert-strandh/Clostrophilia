(cl:in-package #:clostrophilia)

(defgeneric argument-precedence-order (condition))

(define-condition argument-precedence-order-given-but-not-lambda-list
    (program-error)
  ((%argument-precedence-order
    :initarg :argument-precedence-order
    :reader argument-precedence-order))
  (:report (lambda (condition stream)
             (format stream
                     "When the keyword argument :ARGUMENT-PRECEDENCE-ORDER~@
                      is given, then the :LAMBDA-LIST argument must also~@
                      be given, but only the :ARGUMENT-PRECEDENCE-ORDER~@
                      was given:~@
                      ~s"
                     (argument-precedence-order condition)))))
