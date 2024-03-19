(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_slt_un.htm#slot-unbound
(defgeneric slot-unbound (class object slot-name))

(defmethod slot-unbound (class object slot-name)
  (declare (ignore class))
  (error 'unbound-slot
         :name slot-name
         :instance object))
