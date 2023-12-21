(cl:in-package #:clostrophilia)

(define-condition unknown-method-combination (error)
  ((%name :initarg :name :reader name))
  (:report
   (lambda (condition stream)
     (format stream
             "An attempt was made to find the method combination named~@
              ~s~@
              But there is no method condition by that name."
             (name condition)))))

(define-condition order-must-be-most-specific-first-or-last (error)
  ((%order :initarg :order :reader order))
  (:report
   (lambda (condition stream)
     (format stream
             "Order must be :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST,~@
              but the following was found instead:~@
              ~s"
             (order condition)))))

(define-condition method-qualifier-does-not-match (error)
  ((%qualifier :initarg :qualifier :reader qualifier)
   (%group-specifiers :initarg :group-specifiers :reader group-specifiers))
  (:report
   (lambda (condition stream)
     (format stream
             "The method qualfier:~@
              ~s~@
              do not match any of the method group specifiers:~@
              ~s"
             (qualifier condition)
             (group-specifiers condition)))))

(define-condition options-must-be-proper-list (error)
  ((%options :initarg :options :reader options))
  (:report
   (lambda (condition stream)
     (format stream
             "Options must be a proper list, but the following~@
              was found instead:~@
              ~s"
             (options condition)))))

(define-condition options-must-have-an-even-number-of-elements (error)
  ((%options :initarg :options :reader options))
  (:report
   (lambda (condition stream)
     (format stream
             "Options must have an even number of elements,~@
              but the following was found instead:~@
              ~s"
             (options condition)))))

(define-condition option-documentation-given-more-than-once (error)
  ((%options :initarg :options :reader options))
  (:report
   (lambda (condition stream)
     (format stream
             "The :DOCUMENTATION option was given more than once in:~@
              ~s"
             (options condition)))))

(define-condition option-identity-with-one-argument-given-more-than-once
    (error)
  ((%options :initarg :options :reader options))
  (:report
   (lambda (condition stream)
     (format stream
             "The :IDENTITY-WITH-ONE-ARGUMENT option was given~@
              more than once in:~@
              ~s"
             (options condition)))))

(define-condition option-operator-given-more-than-once (error)
  ((%options :initarg :options :reader options))
  (:report
   (lambda (condition stream)
     (format stream
             "The :OPERATOR option was given more than once in:~@
              ~s"
             (options condition)))))

(define-condition method-group-specifier-must-be-proper-list (error)
  ((%group-specifier :initarg :group-specifier :reader group-specifier))
  (:report
   (lambda (condition stream)
     (format stream
             "Method group specifier must be a proper list,~@
              but the following was found instead:~@
              ~s"
             (group-specifier condition)))))

(define-condition name-of-method-group-specifier-must-be-symbol (error)
  ((%name :initarg :name :reader name))
  (:report
   (lambda (condition stream)
     (format stream
             "The name of a method group specifier must be a symbol,~@
              but the following was found instead:~@
              ~s"
             (name condition)))))

(define-condition invalid-method-group-specifier (error)
  ((%group-specifier :initarg :group-specifier :reader group-specifier))
  (:report
   (lambda (condition stream)
     (format stream
             "The name of a method group specifier must be followed,~@
              by a predicate or by at least one qualifier pattern,~@
              but the name is followed by nothing in:~@
              ~s"
             (group-specifier condition)))))
