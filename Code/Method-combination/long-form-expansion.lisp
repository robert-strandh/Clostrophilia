(cl:in-package #:clostrophilia)

(defun long-form-lambda
    (lambda-list-variables method-group-specifiers declarations body)
  (let ((method-list-var (gensym "method-list")))
    `(lambda (,method-list-var ,@lambda-list-variables)
       ,@declarations
       ,(wrap-body method-list-var method-group-specifiers body))))

;;; We do not support the :ARGUMENTS and :GENERIC-FUNCTION options
;;; yet.
;;;
;;; FIXME: We need a way to pass a CLIENT argument to
;;; FIND-METHOD-COMBINATION-TEMPLATE.  Right now, we are passing NIL.
(defun long-form-expander (name lambda-list method-group-specifiers body)
  (let ((lambda-list-variables
          (ecclesia:extract-lambda-list-variables lambda-list)))
    (multiple-value-bind (declarations documentation forms)
        (ecclesia:separate-function-body body)
      `(progn (ensure-method-combination-template
               ',name
               (lambda ,lambda-list
                 (list ,@lambda-list-variables))
               ,(long-form-lambda
                 lambda-list-variables
                 method-group-specifiers
                 declarations
                 forms)
               :documentation ,documentation)
              ',name))))
