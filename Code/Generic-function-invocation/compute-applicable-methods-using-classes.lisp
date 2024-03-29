(cl:in-package #:clostrophilia)

;;; We assume that this code is executed to create functions (ordinary
;;; and generic) and methods with refinement R.  These functions take
;;; as arguments generic functions and methods of refinement R+1.
;;;
;;; When a method of refinement R+1 was created with an EQL
;;; specializer, the object of the EQL specializer is compared to an
;;; argument of a generic function of refinement R+1, which means that
;;; the object has refinement R+2.  So in order to determine the class
;;; of such an object, we need to call the function CLASS-OF of
;;; refinement R+1.  That function is therefore referred to here as
;;; CLASS-OF+1.

;;; We use MEMBER (rather than (say) FIND) because MEMBER is a rather
;;; simple function that works only on lists, whereas we might want to
;;; make FIND a generic function.
(defun subclassp (class1 class2)
  (member class2 (class-precedence-list class1) :test #'eq))

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
                 ((let ((object (eql-specializer-object specializer)))
                    (eq (class-of+1 object) class))
                  (setf result :sometimes))
                 (t
                  (return-from maybe-applicable-p nil)))
        finally (return result)))

;;; Determine whether a method is more specific than another method
;;; with respect to a list of classes of required arguments.
;;;
;;; Recall that whether a method is more or less specific than another
;;; method is also a function of the classes of the arguments, because
;;; the order of two classes in the class precedence list of two
;;; different argument classes can be different.
;;;
;;; This function is called only with applicable methods with respect
;;; to the classes of the arguments supplied.
;;;
;;; It is possible for two methods of a generic function to be equally
;;; specific (which then means that they have the same specializer in
;;; every required position), but then they must have different
;;; qualifiers.  This function is called with all applicable
;;; functions, independent of the qualifiers, so this situation might
;;; happen here.

(defun method-more-specific-p (method1 method2 classes-of-arguments indices)
  (loop with specializers1 = (method-specializers method1)
        with specializers2 = (method-specializers method2)
        for index in indices
        for s1 = (nth index specializers1)
        for s2 = (nth index specializers2)
        for class-of-argument = (nth index classes-of-arguments)
        unless (eq s1 s2)
          return (sub-specializer-p s1 s2 class-of-argument)))

(defun precedence-indices (lambda-list precedence-order)
  (let ((required (subseq lambda-list 0 (length precedence-order))))
    (loop for parameter in precedence-order
          collect (position parameter required))))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-applicable-methods-using-classes.html
(defgeneric compute-applicable-methods-using-classes
    (generic-function classes-of-arguments))

;;; Given a list of classes of the required arguments of a standard
;;; generic function, compute the applicable methods, independently of
;;; their qualifiers, but sorted in order from most to least specific.
;;;
;;; The applicable methods are found by filtering out methods for
;;; which every specializer is a (non-strict) subclass of the
;;; corresponding argument class.  Then they are sorted according to
;;; the order determined by METHOD-MORE-SPECIFIC-P as defined above.
;;;
;;; We extract the body of this method into a separate function,
;;; because there is a potential metastability problem here, and that
;;; is when the function COMPUTE-APPLICABLE-METHODS-USING-CLASSES is
;;; called with the function COMPUTE-APPLICABLE-METHODS-USING-CLASSES
;;; itself as an argument.  We avoid the metastability problem by
;;; recognizing that COMPUTE-APPLICABLE-METHODS-USING-CLASSES is a
;;; standard generic function and having the discriminating function
;;; of COMPUTE-APPLICABLE-METHODS-USING-CLASSES test for the class of
;;; its argument being the class named STANDARD-GENERIC-FUNCTION.
;;; Since portable programs are not allowed to add a method to the
;;; function COMPUTE-APPLICABLE-METHODS-USING-CLASSES unless that
;;; method has a specializer other than STANDARD-GENERIC-FUNCTION or
;;; any of its superclasses, we know that in this case, only the
;;; method below is applicable, so the discriminating function will
;;; call the extracted method body directly.
;;;
;;; The special case for the discriminating function is introduced by
;;; COMPUTE-DISCRIMINATING-FUNCTION, so there is no trace of it here.

;;; The specification includes a single method on this generic
;;; function, specialized for STANDARD-GENERIC-FUNCTION.
(defmethod compute-applicable-methods-using-classes
    ((generic-function standard-generic-function) classes-of-arguments)
  (let* ((lambda-list (generic-function-lambda-list generic-function))
         (profile (specializer-profile generic-function))
         (precedence-order
           (generic-function-argument-precedence-order generic-function))
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

; LocalWords:  specializer
