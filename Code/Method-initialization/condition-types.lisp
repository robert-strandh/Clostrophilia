(cl:in-package #:clostrophilia)

(defgeneric qualifier (condition))

(define-condition qualifier-must-be-non-nil-atom
    (error)
  ((%qualifier :initarg :qualifier :reader qualifier))
  (:report (lambda (condition stream)
             (format stream
                     "A qualifier must be a non-nil atom,~@
                      but the following was found instead:~@
                      ~s"
                     (qualifier condition)))))

(defgeneric qualifiers (condition))

(define-condition qualifiers-must-be-proper-list
    (error)
  ((%qualifiers :initarg :qualifiers :reader qualifiers))
  (:report (lambda (condition stream)
             (format stream
                     "The list of qualifiers must be a proper list,~@
                      but the following was found instead:~@
                      ~s"
                     (qualifiers condition)))))
