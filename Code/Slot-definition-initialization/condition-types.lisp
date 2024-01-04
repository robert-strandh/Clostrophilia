(cl:in-package #:clostrophilia)

(define-condition slot-definition-name-must-be-supplied (program-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "The NAME keyword argument must be supplied"))))

(define-condition slot-definition-name-must-be-symbol (program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "A slot-definition name must be a symbol, but~@
                      the following was supplied instead:~@
                      ~s"
                     (name condition)))))

(defgeneric initform (condition))

(define-condition initform-supplied-but-not-initfunction (program-error)
  ((%initform :initarg :initform :reader initform))
  (:report (lambda (condition stream)
             (format stream
                     "When the :INITFORM keyword argument is supplied,~@
                      the :INITFUNCTION keyword argument must also be~@
                      supplied, but the :INITFUNCTION keyword argument was~@
                      not supplied, wheras the following :INITFORM~@
                      keyword argument was supplied:~@
                      ~s"
                     (initform condition)))))

(defgeneric initfunction (condition))

(define-condition initfunction-supplied-but-not-initform (program-error)
  ((%initfunction :initarg :initfunction :reader initfunction))
  (:report (lambda (condition stream)
             (format stream
                     "When the :INITFUNCTION keyword argument is supplied,~@
                      the :INITFORM keyword argument must also be~@
                      supplied, but the :INITFORM keyword argument was~@
                      not supplied, wheras the following :INITFUNCTION~@
                      keyword argument was supplied:~@
                      ~s"
                     (initfunction condition)))))

(defgeneric allocation (condition))

(define-condition allocation-must-be-symbol (program-error)
  ((%allocation :initarg :allocation :reader allocation))
  (:report (lambda (condition stream)
             (format stream
                     "The :ALLOCATION keyword argument must be a symbol,~@
                      but the following was supplied instead:~@
                      ~s"
                     (allocation condition)))))

(defgeneric initargs (condition))

(define-condition initargs-must-be-proper-list (program-error)
  ((%initargs :initarg :initargs :reader initargs))
  (:report (lambda (condition stream)
             (format stream
                     "The :INITARGS keyword argument must be a proper~@
                      list, but the following was supplied instead:~@
                      ~s"
                     (initargs condition)))))

(defgeneric initarg (condition))

(define-condition initarg-must-be-symbol (program-error)
  ((%initarg :initarg :initarg :reader initarg))
  (:report (lambda (condition stream)
             (format stream
                     "An element of the list supplied as the :INITARGS~@
                      keyword argument must be a symbol, but the~@
                      following was supplied instead:~@
                      ~s"
                     (initarg condition)))))

(defgeneric slot-definition (condition))

(define-condition slot-definition-can-not-be-reinitialized (program-error)
  ((%slot-definition :initarg :slot-definition :reader slot-definition))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to reinitialize a slot-definition metaobject:~@
                      ~s"
                     (slot-definition condition)))))

(defgeneric readers (condition))

(define-condition readers-supplied-but-slot-definition-is-not-direct
    (program-error type-error)
  ((%readers :initarg :readers :reader readers))
  (:default-initargs :expected-type 'DIRECT-SLOT-DEFINITION)
  (:report (lambda (condition stream)
             (format stream
                     "When the :READERS keyword argument is supplied,~@
                      the slot definition must be an instance of the~@
                      class DIRECT-SLOT-DEFINITION, but following~@
                      :READERS argument was supplied:~@
                      ~s~@
                      for the following slot definition:~@
                      ~s"
                     (readers condition)
                     (type-error-datum condition)))))

(define-condition readers-must-be-proper-list (program-error)
  ((%readers :initarg :readers :reader readers))
  (:report (lambda (condition stream)
             (format stream
                     "The :READERS keyword argument must be a proper~@
                      list, but the following was supplied instead:~@
                      ~s"
                     (readers condition)))))

(defgeneric reader (condition))

(define-condition reader-must-be-a-valid-function-name (program-error)
  ((%reader :reader :reader :reader reader))
  (:report (lambda (condition stream)
             (format stream
                     "An element of the list supplied as the :READERS~@
                      keyword argument must be a valid function name,~@
                      but the following was supplied instead:~@
                      ~s"
                     (reader condition)))))

(defgeneric writers (condition))

(define-condition writers-supplied-but-slot-definition-is-not-direct
    (program-error type-error)
  ((%writers :initarg :writers :reader writers))
  (:default-initargs :expected-type 'DIRECT-SLOT-DEFINITION)
  (:report (lambda (condition stream)
             (format stream
                     "When the :WRITERS keyword argument is supplied,~@
                      the slot definition must be an instance of the~@
                      class DIRECT-SLOT-DEFINITION, but following~@
                      :WRITERS argument was supplied:~@
                      ~s~@
                      for the following slot definition:~@
                      ~s"
                     (writers condition)
                     (type-error-datum condition)))))

(define-condition writers-must-be-proper-list (program-error)
  ((%writers :initarg :writers :reader writers))
  (:report (lambda (condition stream)
             (format stream
                     "The :WRITERS keyword argument must be a proper~@
                      list, but the following was supplied instead:~@
                      ~s"
                     (writers condition)))))

(defgeneric writer (condition))

(define-condition writer-must-be-a-valid-function-name (program-error)
  ((%writer :writer :writer :reader writer))
  (:report (lambda (condition stream)
             (format stream
                     "An element of the list supplied as the :WRITERS~@
                      keyword argument must be a valid function name,~@
                      but the following was supplied instead:~@
                      ~s"
                     (writer condition)))))

(define-condition slot-definition-documentation-must-be-string-or-nil
    (error)
  ((%documentation
    :initarg :documentation
    :reader documentation))
  (:report (lambda (condition stream)
             (format stream
                     "A slot-definition :DOCUMENTATION option must be a~@
                      string or NIL, but the following was found instead:~@
                      ~s"
                     (documentation condition)))))
