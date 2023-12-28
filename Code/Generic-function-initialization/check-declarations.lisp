(cl:in-package #:clostrophilia)

;;; FIXME check that each declaration is legal.

(defun check-generic-function-declarations (declarations)
  (unless (ecclesia:proper-list-p declarations)
    (error 'generic-function-declarations-must-be-proper-list
           :declarations declarations)))
