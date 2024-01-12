(cl:in-package #:clostrophilia)

;;; Determine whether a method is applicable to a sequence of argument
;;; classes.  The result can be either T or NIL or :SOMETIMES.  The
;;; result is :SOMETIMES when the method has at least one EQL
;;; specializer, and for each EQL specializer, the class of the
;;; underlying object is identical to the corresponding argument
;;; class.
(defun maybe-applicable-p (method classes profile)
  (loop with result = t
        for specializer in (method-specializers method)
        for class in classes
        for relevant-p in profile
        do (cond ((not relevant-p)
                  nil)
                 ((classp specializer)
                  (unless (subclassp class specializer)
                    (return-from maybe-applicable-p nil)))
                 ((eq (class-of (eql-specializer-object specializer)) class)
                  (setf result :sometimes))
                 (t
                  (return-from maybe-applicable-p nil)))
        finally (return result)))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-applicable-methods-using-classes.html
(defgeneric compute-applicable-methods-using-classes
    (generic-function classes-of-arguments))

;;; The specification includes a single method on this generic
;;; function, specialized for STANDARD-GENERIC-FUNCTION.
(defmethod compute-applicable-methods-using-classes
    ((generic-function standard-generic-function) classes-of-arguments)
  (let* ((lambda-list (generic-function-lambda-list generic-function))
         (profile (specializer-profile generic-function))
         (precedence-order
           (generic-function-argument-precedence-order  generic-function))
         (indices (precedence-indices lambda-list precedence-order))
         (methods (generic-function-methods generic-function))
         (applicability
           (loop for method in methods
                 collect
                 (maybe-applicable-p method classes-of-arguments profile))))
    (if (member :sometimes applicability)
        (return-from compute-applicable-methods-using-classes
          (values '() nil))
        (let* ((applicable-methods
                 (loop for method in methods
                       for applicable-p in applicability
                       when applicable-p
                         collect method))
               (result
                 (sort applicable-methods
                       (lambda (method1 method2)
                         (method-more-specific-p
                          method1
                          method2
                          classes-of-arguments
                          indices)))))
          (values result t)))))
