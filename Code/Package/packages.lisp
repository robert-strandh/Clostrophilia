(cl:in-package #:common-lisp-user)

(defpackage #:clostrophilia
  (:use #:common-lisp)
  (:export
   ;; MOP classes
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
   #:class-direct-superclasses
   #:class-direct-slots
   #:class-direct-default-initargs
   #:class-precedence-list
   #:class-slots
   #:class-default-initargs
   #:class-finalized-p
   #:class-prototype))
