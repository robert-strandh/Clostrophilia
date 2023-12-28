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

(define-condition argument-precedence-order-must-be-proper-list
    (program-error)
  ((%argument-precedence-order
    :initarg :argument-precedence-order
    :reader argument-precedence-order))
  (:report (lambda (condition stream)
             (format stream
                     "The keyword argument :ARGUMENT-PRECEDENCE-ORDER~@
                      must be a proper list, but the following was~@
                      given instead:~@
                      ~s"
                     (argument-precedence-order condition)))))

(defgeneric required (condition))

(define-condition argument-precedence-order-must-be-permutation
    (program-error)
  ((%argument-precedence-order
    :initarg :argument-precedence-order
    :reader argument-precedence-order)
   (%required :initarg :required :reader required))
  (:report (lambda (condition stream)
             (format stream
                     "The keyword argument :ARGUMENT-PRECEDENCE-ORDER~@
                      must be a permutation of the required parameters~@
                      in the lambda list, but the required parameters~@
                      of the lambda list given are:~@
                      ~s~@
                      whereas the given argument precedence order is:~@
                      ~s"
                     (required condition)
                     (argument-precedence-order condition)))))

(define-condition argument-precedence-order-must-contain symbols
    (program-error)
  ((%argument-precedence-order
    :initarg :argument-precedence-order
    :reader argument-precedence-order))
  (:report (lambda (condition stream)
             (format stream
                     "The keyword argument :ARGUMENT-PRECEDENCE-ORDER~@
                      must be a list where every element is a symbol~@
                      but the following was given instead:~@
                      ~s"
                     (argument-precedence-order condition)))))

(defgeneric declarations (condition))

(define-condition generic-function-declarations-must-be-proper-list
    (program-error)
  ((%declarations :initarg :declarations :reader declarations))
  (:report (lambda (condition stream)
             (format stream
                     "The list of declarations of a generic function~@
                      must be a proper list, but the following was~@
                      found instead:~@
                      ~s"
                     (declarations condition)))))

(defgeneric documentation (condition))

(define-condition generic-function-documentation-must-be-nil-or-string
    (program-error)
  ((%documentation-option
    :initarg :documentation
    :reader documentation-option))
  (:report (lambda (condition stream)
             (format stream
                     "The documentation for a generic function must be nil~@
                      or a string.  But the following was found instead:~@
                      ~s"
                     (documentation-option condition)))))
