(cl:in-package #:clostrophilia)

;;; A canonicalized direct slot specification is a property list that
;;; is later used as the list of keyword arguments to some generic
;;; functions.  Some properties are defined by the MOP specification,
;;; but an implementation is free to add more properties.  All we know
;;; is that each property must appear at most once.  If an option can
;;; appear more than once in the slot specifier in the DEFCLASS macro,
;;; which is the case for some of the standardized slot options like
;;; :READER, :WRITER, :ACCESSOR, and :INITARG, then those multiple
;;; options are bundled into a single property in the canonicalized
;;; direct slot specification.  For the standardized options that can
;;; appear multiple times, the corresponding properties are :READERS,
;;; :WRITERS, and :INITARGS.  The name of the slot appears in the
;;; canonicalized direct slot specification as the value of the :NAME
;;; property, which must always be present.
;;;
;;; Furthermore, while the DEFCLASS macro restricts the slot name to
;;; be a symbol that is valid as a variable name, no such restriction
;;; seems to exist in the class initialization protocol.  Perhaps it
;;; is reasonable for some custom slot-definition classes to allow
;;; slot names that are more general.
;;;
;;; So the best we can do to check the contents of a canonicalized
;;; direct slot specification is to make sure it is a proper list with
;;; an even number of elements and that the :NAME property is present.

(defun check-direct-slot-specification (direct-slot-specification)
  (unless (ecclesia:proper-list-p direct-slot-specification)
    (error 'direct-slot-specification-must-be-proper-list
           :direct-slot-specification direct-slot-specification))
  (unless (evenp (length direct-slot-specification))
    (error 'direct-slot-specification-must-be-property-list
           :direct-slot-specification direct-slot-specification))
  
  (let ((default (list nil)))
    (when (eq (getf direct-slot-specification) default)
      (error 'name-property-must-be-present
             :direct-slot-specification direct-slot-specification))))

(defun check-direct-slot-specifications (direct-slot-specifications)
  (unless (ecclesia:proper-list-p direct-slot-specifications)
    (error 'direct-slots-must-be-proper-list
           :direct-slots direct-slot-specifications))
  (mapc #'check-direct-slot-specification direct-slot-specifications))

(defun convert-slot-specification-to-direct-slot-definition
    (class direct-slot-specification)
  (let ((direct-slot-definition-class
          (apply #'direct-slot-definition-class
                 class direct-slot-specification)))
    (apply #'make-instance direct-slot-definition-class
           direct-slot-specification)))

(defun check-and-convert-direct-slot-specifications
    (class direct-slot-specifications)
  (check-direct-slot-specifications direct-slot-specifications)
  (loop for direct-slot-specification in direct-slot-specifications
        collect (convert-slot-specification-to-direct-slot-definition
                 class direct-slot-specification)))
