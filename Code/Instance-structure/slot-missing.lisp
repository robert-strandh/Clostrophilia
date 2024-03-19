(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_slt_mi.htm#slot-missing
(defgeneric slot-missing
    (class object slot-name operation &optional new-value))

(defmethod slot-missing
    (class object slot-name operation &optional new-value)
  (declare (ignore class operation new-value))
  (error 'slot-missing
         :name slot-name
         :object object))
