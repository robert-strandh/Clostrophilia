(cl:in-package #:clostrophilia)

;;; The MOP specification requires the same behavior with respect to
;;; the :LAMBDA-LIST and the :ARGUMENT-PRECEDENCE-ORDER keyword
;;; arguments.  If the :ARGUMENT-PRECEDENCE-ORDER argument is given,
;;; but the :LAMBDA-LIST argument is not, then an error is signaled.
;;; If both are given, then an error is signaled if the value of the
;;; :ARGUMENT-PRECEDENCE-ORDER argument is either not a proper list,
;;; or not a permutation of the required parameters in the
;;; :LAMBDA-LIST argument.  If neither the :LAMBDA-LIST nor the
;;; :ARGUMENT-PRECEDENCE-ORDER argument is given, then no defaulting
;;; occurs.  If the :LAMBDA-LIST argument is given, an error is
;;; signaled if it is not a generic-function lambda list.  If the
;;; :LAMBDA-LIST argument is given, but the :ARGUMENT-PRECEDENCE-ORDER
;;; argument is not, then the :ARGUMENT-PRECEDENCE-ORDER defaults to
;;; the list of required parameters in the lambda list.

(defmethod shared-initialize :around
    ((generic-function generic-function)
     (slot-names t)
     &rest initargs
     &key
       (lambda-list nil lambda-list-p)
       (argument-precedence-order nil argument-precedence-order-p)
     &allow-other-keys)
  (if (not lambda-list-p)
      (if argument-precedence-order-p
          (error 'argument-precedence-order-given-but-not-lambda-list
                 :argument-precedence-order argument-precedence-order)
          (call-next-method))
      (let* ((canonicalized-lambda-list
               (ecclesia:canonicalize-generic-function-lambda-list
                lambda-list))
             (required
               (ecclesia:extract-required canonicalized-lambda-list)))
        (if argument-precedence-order-p
            (progn
              (unless (ecclesia:proper-list-p
                       argument-precedence-order)
                (error 'argument-precedence-order-must-be-proper-list
                       :argument-precedence-order argument-precedence-order))
              (unless (every #'symbolp argument-precedence-order)
                (error 'argument-precedence-order-must-contain-symbols
                       :argument-precedence-order argument-precedence-order))
              (unless (and (= (length required)
                              (length argument-precedence-order))
                           (null (set-difference
                                  argument-precedence-order required))
                           (null (set-difference
                                  required argument-precedence-order)))
                (error 'argument-precedence-order-must-be-permutation
                       :required required
                       :argument-precedence-order argument-precedence-order))
              (call-next-method))
            (apply #'call-next-method
                   generic-function slot-names
                   :argument-precedence-order required
                   initargs)))))
