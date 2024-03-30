(cl:in-package #:clostrophilia)

;;; The MOP specification says that the :DIRECT-DEFAULT-INITARGS
;;; keyword argument must be a proper list of canonicalized default
;;; initialization arguments, and that when a class is initialized, it
;;; defaults to the empty list.
;;;
;;; The MOP specification also says that when a class is initialized,
;;; then if the :DIRECT-SUPERCLASSES keyword argument is either not
;;; given or it is the empty list, then it default depending on the
;;; class being initialized.  We do that by calling another generic
;;; function DEFAULT-SUPERCLASSES with CLASS as its argument.

(defmethod initialize-instance :around
    ((class class)
     &rest initargs
     &key
       (name nil)
       (direct-default-initargs '())
       (direct-slots '())
       (direct-superclasses '())
       (documentation nil)
     &allow-other-keys)
  (let ((defaulted-direct-superclasses
          (if (null direct-superclasses)
              (default-superclasses class)
              direct-superclasses)))
    (let ((*existing-superclasses* '()))
      (apply #'call-next-method
             class
             ;; We supply the DIRECT-DEFAULT-INITARGS argument
             ;; explicitly, in case it was not given.  That way it
             ;; becomes defaulted to the empty list in that case.
             :direct-default-initargs direct-default-initargs
             ;; We supply the DIRECT-SLOTS argument explicitly, in case
             ;; it was not given.  That way it becomes defaulted to the
             ;; empty list in that case.
             :direct-slots direct-slots
             ;; We supply the DOCUMENTATION argument explicitly, in case
             ;; it was not given.  That way it becomes defaulted to NIL
             ;; in that case.
             :documentation documentation
             ;; We supply the NAME argument explicitly, in case it was
             ;; not given.  That way it becomes defaulted to NIL in that
             ;; case.
             :name name
             :direct-superclasses defaulted-direct-superclasses
             initargs))))

(defparameter *class-unique-number* 0)

(defun new-unique-number ()
  (prog1 *class-unique-number* (incf *class-unique-number*)))

(defmethod initialize-instance :after
    ((class built-in-class)
     &rest initargs
     &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (unique-number class) (new-unique-number)))
