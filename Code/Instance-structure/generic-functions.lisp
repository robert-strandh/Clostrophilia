(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-value-using-class.html
(defgeneric slot-value-using-class (class object slot))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-slot-value-using-class.html
(defgeneric (setf slot-value-using-class) (new-value class object slot))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-makunbound-using-class.html
(defgeneric slot-makunbound-using-class (class object slot))
