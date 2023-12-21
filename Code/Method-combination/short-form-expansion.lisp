(cl:in-package #:clostrophilia)

(defun short-form-expander (name options)
  (unless (ecclesia:proper-list-p options)
    (error 'options-must-be-proper-list
           :options options))
  (unless (evenp (length options))
    (error 'options-must-have-an-even-number-of-elements
           :options options))
  (let ((documentation nil)
        (identity-with-one-argument nil)
        (operator nil))
    (loop for (option value) on options by #'cddr
          do (ecase option
               (:documentation
                (unless (null documentation)
                  (error 'option-documentation-given-more-than-once
                         :options options))
                (setf documentation (list value)))
               (:identity-with-one-argument
                (unless (null identity-with-one-argument)
                  (error 'option-identity-with-one-argument-given-more-than-once
                         :options options))
                (setf identity-with-one-argument (list value)))
               (:operator
                (unless (null operator)
                  (error "option :operator given more than once"))
                (setf operator (list value)))))
    (let ((operator (if (null operator) name (car operator))))
      `(define-method-combination ,name
           (&optional (order :most-specific-first))
         ((around (:around))
          (primary (,name) :order order :required t))
         (let ((form (if (null (rest primary))
                         ,(if (car identity-with-one-argument)
                              ``(call-method ,(first primary))
                              ``(,',operator (call-method ,(first primary))))
                         `(,',operator ,@(loop for method in primary
                                               collect `(call-method ,method))))))
           (if (null around)
               form
               `(call-method ,(first around)
                             (,@(rest around)
                              (make-method ,form)))))))))
