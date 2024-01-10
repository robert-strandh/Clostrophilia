(cl:in-package #:clostrophilia)

(defmethod reinitialize-instance :before
    ((slot-definition slot-definition)
     &key &allow-other-keys)
  (error 'slot-definition-can-not-be-reininitialized
         :slot-definition slot-definition))
