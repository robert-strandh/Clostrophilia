(cl:in-package #:clostrophilia)

(define-condition attempt-to-add-existing-subclass
    (error)
  ((%subclass :initarg :subclass :reader subclass)
   (%superclass :initarg :superclass :reader superclass))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to add existing subclass ~s as a subclass of ~s."
                     (subclass condition)
                     (superclass condition)))))

(define-condition readers-must-be-proper-list
    (type-error)
  ((%slot-definition :initarg :slot-definition :reader slot-definition)
   (%readers :initarg :readers :reader readers))
  (:report (lambda (condition stream)
             (format stream
                     "The keyword argument :READERS when supplied as~@
                      initialization of a slot definition, must be~@
                      a proper list, but the following was found instead:~@
                      ~s."
                     (readers condition))))
  (:default-initargs :type 'list))

(define-condition malformed-documentation-option
    (program-error)
  ((%documentation-option
    :initarg :documentation-option :reader documentation-option))
  (:report (lambda (condition stream)
             (format stream
                     "A documentation option must have the form~@
                      (:documentation <name>), but~@
                      ~s was found."
                     (documentation-option condition)))))

(define-condition direct-slots-must-be-proper-list
    (error)
  ((%direct-slots :initarg :direct-slots :reader direct-slots))
  (:report (lambda (condition stream)
             (format stream
                     "The direct slots must be a proper list,~@
                      but the following was found instead:~@
                      ~s"
                     (direct-slots condition)))))

(define-condition direct-slot-specification-must-be-proper-list
    (error)
  ((%direct-slot-specification
    :initarg :direct-slot-specification
    :reader direct-slot-specification))
  (:report (lambda (condition stream)
             (format stream
                     "a direct slot specification must be a proper~@
                      list, but the following was found instead:~@
                      ~s"
                     (direct-slot-specification condition)))))

(define-condition direct-slot-specification-must-be-property-list
    (error)
  ((%direct-slot-specification
    :initarg :direct-slot-specification
    :reader direct-slot-specification))
  (:report (lambda (condition stream)
             (format stream
                     "a direct slot specification must be a property~@
                      list, i.e., a list with an even number of elements,~@
                      but the following was found instead:~@
                      ~s"
                     (direct-slot-specification condition)))))

(define-condition name-property-must-be-present
    (error)
  ((%direct-slot-specification
    :initarg :direct-slot-specification
    :reader direct-slot-specification))
  (:report (lambda (condition stream)
             (format stream
                     "a direct slot specification must contain the :NAME~@
                      property, but the following was found instead:~@
                      ~s"
                     (direct-slot-specification condition)))))

(define-condition direct-default-initargs-must-be-a-proper-list
    (error)
  ((%initargs :initarg :initargs :reader initargs))
  (:report (lambda (condition stream)
             (format stream
                     "The list of direct default initargs must be~@
                      a proper list, but the following was found:~@
                      ~s"
                     (initargs condition)))))

(define-condition direct-default-initarg-must-be-a-proper-list
    (error)
  ((%initarg :initarg :initarg :reader initarg))
  (:report (lambda (condition stream)
             (format stream
                     "A direct default initarg must be a proper~@
                      list, but the following was found:~@
                      ~s"
                     (initarg condition)))))

(define-condition direct-default-initarg-must-be-a-list-of-three-elements
    (error)
  ((%initarg :initarg :initarg :reader initarg))
  (:report (lambda (condition stream)
             (format stream
                     "A direct default initarg must be a list of~@
                      three elements, but the following was found:~@
                      ~s"
                     (initarg condition)))))

(define-condition name-of-direct-default-initarg-must-be-a-symbol
    (error)
  ((%initarg :initarg :initarg :reader initarg)
   (%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "The name of a direct default initarg must be a~@
                      symbol, but the following was found:~@
                      ~s"
                     (name condition)))))

(define-condition third-element-of-direct-default-initarg-must-be-a-thunk
    (error)
  ((%initarg :initarg :initarg :reader initarg)
   (%initfunction :initarg :initfunction :reader initfunction))
  (:report (lambda (condition stream)
             (format stream
                     "The third element of a direct default initarg~@
                      must be a thunk, but the following was found instead~@
                      ~s"
                     (initfunction condition)))))

(define-condition class-can-not-be-superclass (error)
  ((%subclass :initarg :subclass :reader subclass)
   (%superclass :initarg :superclass :reader superclass))
  (:report (lambda (condition stream)
             (format stream
                     "The class~@
                      ~s~@
                      can not be a superclass of the class~@
                      ~s"
                     (superclass condition)
                     (subclass condition)))))

(define-condition class-documentation-option-must-be-string-or-nil
    (error)
  ((%documentation-option
    :initarg :documentation-option
    :reader documentation-option))
  (:report (lambda (condition stream)
             (format stream
                     "A class :DOCUMENTATION option must be a string~@
                      or NIL, but the following was found instead:~@
                      ~s"
                     (documentation-option condition)))))
