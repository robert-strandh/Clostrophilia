(cl:in-package #:clostrophilia)

(defun check-method-qualifiers (method-qualifiers)
  (unless (ecclesia:proper-list-p method-qualifiers)
    (error 'qualifiers-must-be-proper-list
           :qualifiers method-qualifiers))
  (loop for qualifier in method-qualifiers
        unless (and (atom qualifier) (not (null qualifier)))
          do (error 'qualifier-must-be-non-nil-atom
                    :qualifier qualifier)))

(defun check-unspecialized-lambda-list (lambda-list)
  (ecclesia:canonicalize-ordinary-lambda-list lambda-list))

(defun check-specializers (specializers lambda-list)
  (unless (ecclesia:proper-list-p specializers)
    (error 'specializers-must-be-proper-list
           :specializers specializers))
  (let ((required (ecclesia:extract-required lambda-list)))
    (unless (= (length specializers) (length required))
      (error 'incorrect-number-of-specializers
             :specializers specializers
             :lambda-list lambda-list))))

(defmethod initialize-instance :before
    ((method method)
     &key
       (qualifiers '())
       (lambda-list nil lambda-list-p)
       (specializers nil specializers-p)
       (function nil function-p)
     &allow-other-keys)
  (check-method-qualifiers qualifiers)
  (check-unspecialized-lambda-list lambda-list)
  (unless lambda-list-p
    (error 'lambda-list-must-be-supplied))
  (unless specializers-p
    (error 'specializers-must-be-supplied))
  (check-specializers specializers lambda-list))

(defmethod initialize-instance :around
    ((method method)
     &rest initargs
     &key
       (qualifiers '())
       )
  (apply #'call-next-method
         :qualifiers qualifiers
         initargs))
