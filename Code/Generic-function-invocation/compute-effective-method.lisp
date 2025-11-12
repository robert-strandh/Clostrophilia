(cl:in-package #:clostrophilia)

(defun wrap-make-method-form
    (form arguments-var next-methods-var)
  `(lambda (,arguments-var ,next-methods-var)
     (flet ((next-method-p ()
              (not (null ,next-methods-var)))
            (call-next-method (&rest arguments)
              (funcall (method-function (first ,next-methods-var))
                       (if (null arguments)
                           ,arguments-var
                           arguments)
                       (rest ,next-methods-var))))
       ,form)))

;;; The standard says that methods with keyword parameters are called
;;; "as if" :ALLOW-OTHER-METHODS T were passed in addition to other
;;; keyword arguments.  The technique we use here is to literally add
;;; :ALLOW-OTHER-METHODS T in that case, because we don't see any
;;; other technique that will always work, given that the user is free
;;; to create a method using MAKE-INSTANCE and passing it a :FUNCTION
;;; argument that is any old existing function.  A much better
;;; solution would be for methods to have an entry point that does not
;;; parse keyword arguments, and to have the effective method call
;;; this entry point when possible.  but this is a general library
;;; that does not make any assumptions about the representation of
;;; ordinary functions.

(defun funcall-method-function
    (method-function arguments lambda-list next-method)
  (if (member '&key lambda-list)
      (funcall method-function
               (append arguments '(:allow-other-keys t))
               next-method)
      (funcall method-function arguments next-method)))

(defun wrap-in-call-method-macrolet (form arguments-var)
  `(macrolet ((call-method (method &optional next-method-list)
                (cond ((and (consp method)
                            (eq (first method) 'make-method)
                            (null (rest (rest method))))
                       `(funcall ,(compile
                                   nil
                                   `(lambda ()
                                      ,(wrap-in-call-method-macrolet (second method) ',arguments-var)))))
                      ((not (consp method))
                       `(funcall-method-function
                         ,(method-function method)
                         ,',arguments-var
                         ',(method-lambda-list method)
                         (list ,@next-method-list)))
                      (t (error "Malformed argument to CALL-METHOD ~s" method)))))
     ,form))

(defun wrap-in-make-method-macrolet (form method-class)
  `(macrolet ((make-method (make-method-form)
                (let ((arguments-var (gensym))
                      (next-methods-var (gensym)))
                  (make-method-instance ,method-class
                    :qualifiers '()
                    :lambda-list '()
                    :specializers '()
                    :function
                    (compile nil
                     `(lambda (,arguments-var ,next-methods-var)
                        (declare (ignorable ,arguments-var))
                        (declare (ignore ,next-methods-var))
                        ,(wrap-in-call-method-macrolet make-method-form arguments-var)))))))
     ,form))

(defun wrap-method-combination-form (form method-class)
  (let ((arguments-var (gensym "ARGUMENTS-")))
    `(lambda (,arguments-var)
       ,(wrap-in-call-method-macrolet
         (wrap-in-make-method-macrolet form method-class)
         arguments-var))))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-effective-method.html
(defgeneric compute-effective-method
    (generic-function method-combination methods))

(defmethod compute-effective-method
    ((generic-function standard-generic-function) method-combination methods)
  (let* ((method-qualifier-pairs
           (loop for method in methods
                 collect (cons method (method-qualifiers method))))
         (template (template method-combination))
         (variant-signature (variant-signature method-combination))
         (function (effective-method-form-function template))
         (form (apply function method-qualifier-pairs variant-signature))
         (method-class (generic-function-method-class generic-function))
         (result (wrap-method-combination-form form method-class)))
    result))
