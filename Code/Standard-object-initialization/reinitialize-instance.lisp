(cl:in-package #:clostrophilia)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_reinit.htm#reinitialize-instance
(defgeneric reinitialize-instance
    (instance &rest initargs &key &allow-other-keys))

(defmethod reinitialize-instance
    ((instance standard-object) &rest initargs &key &allow-other-keys)
  ;; Call shared-initialize with a slot-list of (), meaning no slot,
  ;; i.e., only assign values to slots that have explicit
  ;; initialization arguments in initargs.
  (apply #'shared-initialize instance () initargs))
