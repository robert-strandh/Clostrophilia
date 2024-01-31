(cl:in-package #:asdf-user)

(defsystem "clostrophilia-method-combination"
  :depends-on ("clostrophilia-package"
               "ecclesia")
  :serial t
  :components
  ((:file "condition-types")))
