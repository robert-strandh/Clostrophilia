(cl:in-package #:clostrophilia)

(defun check-documentation (documentation)
  (unless (or (null documentation)
              (stringp documentaton))
    (error 'class-documentation-option-must-be-string-or-nil
           :documentation-option documentation)))
