(cl:in-package #:clostrophilia)

(defmethod initialize-instance :before
    ((method method)
     &key
       (lambda-list nil lambda-list-p)
       (specializers nil specializers-p)
       (function nil function-p)
     &allow-other-keys)
  )


(defmethod initialize-instance :around
    ((method method)
     &rest initargs
     &key
       (qualifiers '())
       )
  )
