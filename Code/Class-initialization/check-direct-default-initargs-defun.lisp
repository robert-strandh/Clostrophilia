(cl:in-package #:clostrophilia)

;;; The MOP specification says that the argument
;;; :DIRECT-DEFAULT-INITARGS must be a proper list of canonicalized
;;; default initialization arguments.
;;;
;;; It says that an error is signaled if the list of canonicalized
;;; default initialization arguments is not a proper list, so we start
;;; by checking that.
;;;
;;; It then says that an error if signaled if any element of the list
;;; is not a canonicalized default initialization argument.  Recall
;;; that a canonicalized default initialization argument is a list of
;;; three elements.  The first element is the name.  The second
;;; element is the form.  The third element is a function of zero
;;; arguments (so a thunk) which, when called, returns the result of
;;; evaluating the default value form in its proper lexical
;;; environment.

(defun check-direct-default-initarg (direct-default-initarg)
  (unless (ecclesia:proper-list-p direct-default-initarg)
    (error 'direct-default-initarg-must-be-a-proper-list
           :initarg direct-default-initarg))
  (unless (= (length direct-default-initarg) 3)
    (error 'direct-default-initarg-must-be-a-list-of-three-elements
           :initarg direct-default-initarg))
  (destructuring-bind (name form function) direct-default-initarg
    (declare (ignore form))
    (unless (symbolp name)
      (error 'name-of-direct-default-initarg-must-be-a-symbol
             :initarg direct-default-initarg))
    (unless (functionp function)
      (error 'third-element-of-direct-default-initarg-must-be-a-thunk
             :initarg direct-default-initarg))))

(defun check-direct-default-initargs (direct-default-initargs)
  (unless (ecclesia:proper-list-p direct-default-initargs)
    (error 'direct-default-initargs-must-be-a-proper-list
           :initargs direct-default-initargs))
