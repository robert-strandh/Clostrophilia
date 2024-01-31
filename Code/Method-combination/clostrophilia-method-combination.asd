(cl:in-package #:asdf-user)

(defsystem "clostrophilia-method-combination"
  :depends-on ("clostrophilia-package"
               "ecclesia")
  :serial t
  :components
  ((:file "configuration")
   (:file "accessor-defgenerics")
   (:file "method-combination-defclass")
   (:file "method-combination-template-defclass")
   (:file "method-group-specifier")
   (:file "method-discriminator")
   (:file "long-form-expansion")
   (:file "short-form-expansion")
   (:file "find-method-combination-defun")
   (:file "define-method-combination-defmacro")
   (:file "standard-method-combination")
   (:file "built-in-method-combinations")
   (:file "condition-types")))
