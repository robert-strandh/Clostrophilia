(cl:in-package #:clostrophilia)

(defun compute-specializer-profile (existing-profile specializers)
  (loop with class-t = (find-class-t)
        for specializer in specializers
        for p in existing-profile
        collect (if (eq specializer class-t) p t)))

(defun specializer-profile (generic-function)
  (let* ((lambda-list (generic-function-lambda-list generic-function))
         (canonicalized-lambda-list
           (ecclesia:canonicalize-generic-function-lambda-list lambda-list))
         (required (ecclesia:extract-required canonicalized-lambda-list))
         (profile (make-list (length required) :initial-element nil)))
    (loop for method in (generic-function-methods generic-function)
          for specializers = (method-specializers method)
          do (setf profile
                   (compute-specializer-profile profile specializers)))
    profile))
