(cl:in-package #:asdf-user)

(defsystem clostrophilia-class-finalization
  :serial t
  :components
  ((:file "generic-functions")
   (:file "compute-class-precedence-list")))
