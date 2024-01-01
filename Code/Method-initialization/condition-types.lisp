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

(define-condition lambda-list-must-be-supplied
    (error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "The LAMBDA-LIST keyword argument must be supplied"))))

(define-condition specializers-must-be-supplied
    (error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "The SPECIALIZERS keyword argument must be supplied"))))

(defgeneric specializers (condition))

(define-condition specializers-must-be-proper-list
    (error)
  ((%specializers :initarg :specializers :reader specializers))
  (:report (lambda (condition stream)
             (format stream
                     "The list of specializers must be a proper list,~@
                      but the following was found instead:~@
                      ~s"
                     (specializers condition)))))

(define-condition incorrect-number-of-specializers (program-error)
  ((%specializers :initarg :specializers :reader specializers)
   (%lambda-list :initarg :lambda-list :reader lambda-list))
  (:report (lambda (condition stream)
             (format stream
                     "The list of specializers must have as many elements~@
                      as the number of required parameters in the~@
                      lambda list, but the following list of specializers~@
                      was given:~@
                      ~s
                      and the following lambda list was given:~@
                      ~s"
                     (specializers condition)
                     (lambda-list condition)))))

(define-condition function-must-be-supplied
    (error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "The FUNCTION keyword argument must be supplied"))))

(define-condition slot-definition-must-be-supplied
    (error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "The SLOT-DEFINITION keyword argument must be supplied"))))

(define-condition slot-definition-must-be-direct-slot-definition
    (program-error)
  ((%slot-definition :initarg slot-definition :reader slot-definition))
  (:report (lambda (condition stream)
             (format stream
                     "The SLOT-DEFINITION keyword argument must be an~@
                      instance of a subcalss of DIRECT-SLOT-DEFINITION,~@
                      but the following was given instead:~@
                      ~s"
                     (slot-definition condition)))))

(define-condition method-documentation-option-must-be-string-or-nil
    (error)
  ((%documentation-option
    :initarg :documentation-option
    :reader documentation-option))
  (:report (lambda (condition stream)
             (format stream
                     "A method :DOCUMENTATION option must be a string~@
                      or NIL, but the following was found instead:~@
                      ~s"
                     (documentation-option condition)))))

(define-condition method-can-not-be-reinitialized (program-error)
  ((%method-object :initarg :method-object :reader method-object))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to reinitialize a method metaobject:~@
                      ~s"
                     (method-object condition)))))
