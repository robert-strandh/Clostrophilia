(cl:in-package #:clostrophilia)

(defun check-method-qualifiers (method-qualifiers)
  (unless (ecclesia:proper-list-p method-qualifiers)
    (error 'qualifiers-must-be-proper-list
           :qualifiers method-qualifiers))
  (loop for qualifier in method-qualifiers
        unless (and (atom qualifier) (not (null qualifier)))
          do (error 'qualifier-must-be-non-nil-atom
                    :qualifier qualifier)))

(defmethod initialize-instance :before
    ((method method)
     &key
       (lambda-list nil lambda-list-p)
       (specializers nil specializers-p)
       (function nil function-p)
     &allow-other-keys)
  (check-method-qualifiers qualifiers))


(defmethod initialize-instance :around
    ((method method)
     &rest initargs
     &key
       (qualifiers '())
       )
  (apply #'call-next-method
         :qualifiers qualifiers
         initargs))
