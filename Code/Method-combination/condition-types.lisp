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
