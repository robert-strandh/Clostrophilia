(cl:in-package #:clostrophilia)

;;; We assume that this code is executed to create functions (ordinary
;;; and generic) and methods with refinement R.  These functions
;;; operate on generic functions and methods of refinement R+1.
;;;
;;; They also operate on the arguments to the generic functions of
;;; refinement R+1, and these arguments are of refinement R+2.  When
;;; we need to know the classes of those arguments, we must call
;;; CLASS-OF of refinement R+1.  That function is therefore referred
;;; to here as CLASS-OF+1.
;;;
;;; The specializers of a method being operated on are of refinement
;;; R+1 just like the method itself.  So when we want to compare such
;;; a specializer to the class named T, that class must be the class
;;; of that name of refinement R+1.  Rather than calling FIND-CLASS of
;;; refinement R+1 to obtain that class, we introduce a special
;;; variable *CLASS-T+1* to hold that class.

(defvar *class-t+1*)

;;; Determine whether a method is applicable to a sequence of
;;; arguments.  The list of arguments may contain more elements than
;;; there are required parameters, and in that case the remaining
;;; elements of the list of arguments are simply ignored.
(defun definitely-applicable-p (method arguments classes)
  (loop for specializer in (method-specializers method)
        for argument in arguments
        for class in classes
        do (if (classp specializer)
               (unless (eq specializer *class-t+1*)
                 (unless (subclassp class specializer)
                   (return-from definitely-applicable-p nil)))
               (unless (eql (eql-specializer-object specializer) argument)
                 (return-from definitely-applicable-p nil)))
        finally (return t)))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-applicable-methods.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_comput.htm#compute-applicable-methods
(defgeneric compute-applicable-methods (generic-function arguments))

;;; Given a list of arguments of a standard generic function, compute
;;; the applicable methods, independently of their qualifiers, but
;;; sorted in order from most to least specific.
;;;
;;; We extract the body of this method into a separate function,
;;; because there is a potential metastability problem here, and that
;;; is when the function COMPUTE-APPLICABLE-METHODS is called with the
;;; function COMPUTE-APPLICABLE-METHODS itself as an argument.  We
;;; avoid the metastability problem by recognizing that
;;; COMPUTE-APPLICABLE-METHODS is a standard generic function and
;;; having the discriminating function of COMPUTE-APPLICABLE-METHODS
;;; test for the class of its argument being the class named
;;; STANDARD-GENERIC-FUNCTION.  Since portable programs are not
;;; allowed to add a method to the function COMPUTE-APPLICABLE-METHODS
;;; unless that method has a specializer other than
;;; STANDARD-GENERIC-FUNCTION or any of its superclasses, we know that
;;; in this case, only the method below is applicable, so the
;;; discriminating function will call the extracted method body
;;; directly.
;;;
;;; The special case for the discriminating function is introduced by
;;; COMPUTE-DISCRIMINATING-FUNCTION, so there is no trace of it here.

;;; The specification includes a single method on this generic
;;; function, specialized to STANDARD-GENERIC-FUNCTION.
(defmethod compute-applicable-methods
    ((generic-function standard-generic-function) arguments)
  (let* ((profile (specializer-profile generic-function))
         (required-argument-count (length profile))
         (required-arguments (subseq arguments 0 required-argument-count))
         (classes-of-arguments
           (loop for argument in required-arguments
                 for p in profile
                 collect (if p (class-of+1 argument) *class-t+1*)))
         (lambda-list (generic-function-lambda-list generic-function))
         (precedence-order
           (generic-function-argument-precedence-order generic-function))
         (methods (generic-function-methods generic-function))
         (indices (precedence-indices lambda-list precedence-order)))
    (let ((result (sort
                   (loop for method in methods
                         when (definitely-applicable-p
                                  method arguments classes-of-arguments)
                           collect method)
                   (lambda (method1 method2)
                     (method-more-specific-p
                      method1
                      method2
                      classes-of-arguments
                      indices)))))
      result)))

; LocalWords:  specializers specializer
