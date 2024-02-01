(cl:in-package #:asdf-user)

(defsystem "clostrophilia-method-combination-condition-types"
  :depends-on ("clostrophilia-package"
               "ecclesia")
  :serial t
  :components
  ((:file "condition-types")))
