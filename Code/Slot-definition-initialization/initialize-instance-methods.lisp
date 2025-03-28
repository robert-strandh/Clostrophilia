(cl:in-package #:clostrophilia)

(defun valid-function-name-p (function-name)
  (or (symbolp function-name)
      (and (consp function-name)
           (eql (car function-name) 'setf)
           (consp (cdr function-name))
           (symbolp (cadr function-name))
           (null (cddr function-name)))))

(defmethod initialize-instance :before
    ((slot-definition slot-definition)
     &key
       (name nil name-p)
       (initform nil initform-p)
       (initfunction nil initfunction-p)
       (allocation nil)
       (initargs '())
       (readers '() readers-p)
       (writers '() writers-p)
       (documentation nil)
     &allow-other-keys)
  (unless name-p
    (error 'slot-definition-name-must-be-supplied))
  (unless (symbolp name)
    (error 'slot-definition-name-must-be-symbol
           :name name))
  (when (and initform-p (not initfunction-p))
    (error 'initform-supplied-but-not-initfunction
           :initform initform))
  (when (and (not initform-p) initfunction-p)
    (error 'initfunction-supplied-but-not-initform
           :initfunction initfunction))
  (unless (symbolp allocation)
    (error 'allocation-must-be-symbol
           :allocation allocation))
  (unless (ecclesia:proper-list-p initargs)
    (error 'initargs-must-be-proper-list
           :initargs initargs))
  (loop for initarg in initargs
        unless (symbolp initarg)
          do (error 'initarg-must-be-symbol
                    :initarg initarg))
  (when (and readers-p (not (direct-slot-definition-p slot-definition)))
    (error 'readers-supplied-but-slot-definition-is-not-direct
           :readers readers
           :datum slot-definition))
  (unless (ecclesia:proper-list-p readers)
    (error 'readers-must-be-proper-list
           :readers readers))
  (loop for reader in readers
        unless (valid-function-name-p reader)
          do (error 'reader-must-be-a-valid-function-name
                    :reader reader))
  (when (and writers-p (not (direct-slot-definition-p slot-definition)))
    (error 'writers-supplied-but-slot-definition-is-not-direct
           :writers writers
           :datum slot-definition))
  (unless (ecclesia:proper-list-p writers)
    (error 'writers-must-be-proper-list
           :writers writers))
  (loop for writer in writers
        unless (valid-function-name-p writer)
          do (error 'writer-must-be-a-valid-function-name
                    :writer writer))
  (unless (or (null documentation) (stringp documentation))
    (error 'slot-definition-documentation-must-be-string-or-nil
           :documentation documentation)))

(defmethod initialize-instance :around
    ((slot-definition slot-definition)
     &rest keyword-arguments
     &key
       (initform nil)
       (initfunction nil)
       (type t)
       (allocation :instance)
       (initargs '())
       (documentation nil)
     &allow-other-keys)
  (apply #'call-next-method
         slot-definition
         :initform initform
         :initfunction initfunction
         :type type
         :allocation allocation
         :initargs initargs
         :documentation documentation
         keyword-arguments))

(defmethod initialize-instance :around
    ((slot-definition direct-slot-definition)
     &rest keyword-arguments
     &key
       (readers '())
       (writers '())
     &allow-other-keys)
  (apply #'call-next-method
         slot-definition
         :readers readers
         :writers writers
         keyword-arguments))
