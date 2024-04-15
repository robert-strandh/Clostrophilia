(cl:in-package #:asdf-user)

(defsystem "clostrophilia-method-combination-base"
  :depends-on ("clostrophilia-package"
               "ecclesia")
  :serial t
  :components
  ((:file "accessor-defgenerics")
   (:file "method-combination-template-defclass")
   (:file "ensure-method-combination")))
