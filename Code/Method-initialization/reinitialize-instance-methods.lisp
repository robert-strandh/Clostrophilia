(cl:in-package #:clostrophilia)

(defmethod reinitialize-instance :before
    ((method method)
     &key &allow-other-keys)
  (error 'method-can-not-be-reininitialized
         :method-object method))
