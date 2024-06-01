(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-argument-precedence-order.html
(defgeneric generic-function-argument-precedence-order (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-declarations.html
(defgeneric generic-function-declarations (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-method-class.html
(defgeneric generic-function-method-class (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-method-combination.html
(defgeneric generic-function-method-combination (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-methods.html
(defgeneric generic-function-methods (generic-function))

;;; This function sets the methods of the generic function.
(defgeneric (setf generic-function-methods) (new-methods generic-function))

(defgeneric initial-methods (generic-function))

(defgeneric (setf initial-methods) (initial-methods generic-function))

;;; This function returns the call history of the generic function.
;;; FIXME: say more.
(defgeneric call-history (generic-function))

;;; This function sets the call history of the generic function.
;;; FIXME: say more.
(defgeneric (setf call-history) (new-call-history generic-function))

(defclass standard-generic-function (generic-function)
  ((%argument-precedence-order
    :initarg :argument-precedence-order
    :reader generic-function-argument-precedence-order)
   (%specializer-profile
    :initarg :specializer-profile)
   (%declarations 
    :initarg :declarations
    :reader generic-function-declarations)
   (%method-class 
    :initarg :method-class
    :reader generic-function-method-class)
   (%method-combination 
    :initarg :method-combination
    ;; FIXME: remove this later.
    :initform nil
    :reader generic-function-method-combination)
   ;; This slot contains all the methods of the generic function,
   ;; including those that were defined as part of the DEFGENERIC
   ;; form.
   (%methods 
    :initform '() 
    :accessor generic-function-methods)
   ;; This slot contains only the methods that were defined as part of
   ;; the DEFGENERIC form.  We need it, because the Common Lisp
   ;; HyperSpec says that when a DEFGENERIC form is evaluated and the
   ;; generic function exists already, then the methods that were
   ;; added as a result of the evaluation of the DEFGENERIC form are
   ;; first removed.  That is not the case for methods defined by
   ;; separate DEFMETHOD forms.
   (%initial-methods
    :initform '()
    :accessor initial-methods)
   ;; We maintain a CALL HISTORY of the generic function.  This call
   ;; history is a list of call records.  Whenever a call is made to
   ;; the generic function with some call profile that has not yet
   ;; been used in a call, we compute the effective method to use, and
   ;; we add a call record to the call history.
   (%call-history 
    :initform '() 
    :accessor call-history))
  (:metaclass funcallable-standard-class)
  (:default-initargs
   :declarations '()
   :method-class (find-class 'standard-method)))

;;  LocalWords:  DEFGENERIC defgeneric
