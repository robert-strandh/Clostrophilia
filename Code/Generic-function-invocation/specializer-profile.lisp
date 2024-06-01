(cl:in-package #:clostrophilia)

(defun compute-specializer-profile (existing-profile specializers)
  (loop with class-t = (find-class-t)
        for specializer in specializers
        for p in existing-profile
        collect (if (eq specializer class-t) p t)))

;;; Update the specializer profile of a generic function according to
;;; a list of specializers of a method.
(defun update-specializer-profile (generic-function specializers)
  (setf (specializer-profile generic-function)
        (compute-specializer-profile
         (specializer-profile generic-function) specializers)))

;;; Compute a completely new specializer profile for a generic
;;; function.
(defun compute-and-set-specializer-profile (generic-function)
  ;; Keep the length of the profile, but with all elements NIL.
  (setf (specializer-profile generic-function)
        (make-list (length (specializer-profile generic-function))))
  (loop for method in (generic-function-methods generic-function)
        for specializers = (method-specializers method)
        do (update-specializer-profile generic-function specializers)))
