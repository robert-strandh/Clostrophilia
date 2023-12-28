(cl:in-package #:clostrophilia)

(defparameter *qualities*
  '(compilation-speed debug safety space speed))

(defun valid-quality-p (quality)
  (or (member quality *qualities*)
      (and (ecclesia:proper-list-p quality)
           (= (length quality 2))
           (member (first quality) *qualities*)
           (typep (second quality) '(integer 0 3)))))

(defun check-generic-function-declaration (declaration)
  (unless (and (ecclesia:proper-list-p declaration)
               (eq (first declaration) 'optimize)
               (every #'valid-quality-p (rest declaration)))
    (error 'generic-function-declaration-must-be-valid
           :declaration declaration)))

(defun check-generic-function-declarations (declarations)
  (unless (ecclesia:proper-list-p declarations)
    (error 'generic-function-declarations-must-be-proper-list
           :declarations declarations))
  (mapc #'check-generic-function-declaration declarations))
