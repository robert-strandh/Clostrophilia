(cl:in-package #:clostrophilia)

;;; This generic function is called by the function
;;; FIND-METHOD-COMBINATION defined in this system.  The function
;;; FIND-METHOD-COMBINATION has a first parameter CLIENT which must be
;;; supplied by client code, and the CLIENT argument is supplied to
;;; the call of this function.  Client code must define a method on
;;; this function specialized to the class of its own CLIENT object.

(defgeneric find-method-combination-template (client name))
