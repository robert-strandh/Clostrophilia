(cl:in-package #:clostrophilia)

(defun check-generic-function-documentation (documentation)
  (unless (or (null documentation) (stringp documentation))
    (error 'generic-function-documentation-must-be-nil-or-string
           :documentation documentation)))
