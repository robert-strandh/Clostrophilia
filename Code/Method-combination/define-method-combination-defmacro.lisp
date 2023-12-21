(cl:in-package #:clostrophilia)

(defmacro define-method-combination (name &rest more-arguments)
  (if (or (null more-arguments)
          (and (symbolp (first more-arguments))
               (not (null (first more-arguments)))))
      (short-form-expander name more-arguments)
      (destructuring-bind (lambda-list method-group-specifiers . body)
          more-arguments
        (long-form-expander name
                            lambda-list
                            method-group-specifiers
                            body))))
