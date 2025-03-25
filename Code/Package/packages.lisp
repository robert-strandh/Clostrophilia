(cl:in-package #:common-lisp-user)

(defpackage #:clostrophilia
  (:use #:common-lisp)
  (:shadow #:documentation)
  (:export
   ;; MOP classes.
   #:regular-class #:real-class
   #:class #:standard-class #:built-in-class #:structure-class
   #:forward-referenced-class
   #:funcallable-standard-class
   #:standard-object #:function #:funcallable-standard-object
   #:simple-function
   #:generic-function #:standard-generic-function
   #:method #:standard-method
   #:metaobject
   #:specializer
   #:eql-specializer
   #:method-combination
   #:method-combination-template
   #:slot-definition
   #:direct-slot-definition
   #:effective-slot-definition
   #:standard-slot-definition
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-reader-method
   #:standard-writer-method
   ;; Accessors for class metaobjects. 
   #:class-name
   #:class-direct-subclasses
   #:class-direct-superclasses
   #:class-direct-slots
   #:class-direct-default-initargs
   #:class-precedence-list
   #:class-slots
   #:class-default-initargs
   #:class-finalized-p
   #:class-prototype
   ;; Accessors for generic-function metaobjects
   #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-combination
   #:generic-function-method-class
   #:generic-function-name
   ;; Accessors for method metaobjects
   #:method-generic-function
   #:method-function
   #:method-lambda-list
   #:method-specializers
   #:method-qualifiers
   #:accessor-method-slot-definition
   ;; Accessors for slot-definition metaobjects
   #:slot-definition-name
   #:slot-definition-type
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-location
   #:slot-definition-allocation
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   ;; Condition types.
   #:unknown-method-combination
   #:order-must-be-most-specific-first-or-last
   #:method-qualifier-does-not-match
   #:options-must-be-proper-list
   #:options-must-have-an-even-number-of-elements
   #:option-documentation-given-more-than-once
   #:option-identity-with-one-argument-given-more-than-once
   #:option-operator-given-more-than-once
   #:method-group-specifier-must-be-proper-list
   #:name-of-method-group-specifier-must-be-symbol
   #:no-applicable-method-error
   ;; Dependent maintenance
   #:add-dependent
   #:remove-dependent
   #:map-dependents
   #:update-dependent
   ;; Direct subclasses
   #:add-direct-subclass
   #:remove-direct-subclass
   ;; Direct methods
   #:add-direct-method
   #:remove-direct-methods
   #:specializer-direct-methods
   ;; Diverse
   #:reader-method-class
   #:writer-method-class
   #:add-method
   #:remove-method
   #:allocate-instance #:allocate-instance-using-class
   #:set-funcallable-instance-function
   #:finalize-inheritance
   #:find-method-combination-template
   #:find-method-combination
   #:classp
   #:small-integer=
   #:small-integer<))
