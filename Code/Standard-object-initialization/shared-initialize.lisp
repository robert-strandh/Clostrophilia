(cl:in-package #:clostrophilia)

(defgeneric shared-initialize
    (object slot-names &rest initargs &key &allow-other-keys))

(defmethod shared-initialize
    ((instance standard-object) slot-names &rest initargs)
  (let* ((class (class-of instance))
         (slots (class-slots class)))
    (loop for slot in slots
          do ;; (multiple-value-bind (key value foundp)
             ;;     ;; Find the first key/value pair in initargs where
             ;;     ;; the key is one of the initargs of the slot.
             ;;     (get-properties initargs (slot-definition-initargs slot))
             ;;   (declare (ignore key))
             ;;   (format *trace-output* "here 3~%")
             ;;
             ;; The following loop replaces the commented-out code
             ;; above.  The reason is that MUTLIPE-VALUE-BIND might
             ;; expand to MUTLIPLE-VALUE-CALL which in turn creates a
             ;; closure, and allocating a closure triggers the
             ;; object-initialization protocol, which lands us in an
             ;; infinite recursion.
             (let ((value nil)
                   (foundp nil))
               (loop with slot-initargs = (slot-definition-initargs slot)
                     for (i v) on initargs by #'cddr
                     when (member i slot-initargs)
                       do (setf value v foundp t)
                          (loop-finish))
               (if foundp
                   ;; Found an explicit initarg in initargs.
                   ;; Initialize the slot from its value.
                   (setf (slot-value-using-class class instance slot)
                         value)
                   ;; No explicit initarg found.
                   (when (and (not (slot-boundp-using-class
                                    class instance slot))
                              (not (null (slot-definition-initfunction slot)))
                              (or (eq slot-names t)
                                  (member (slot-definition-name slot)
                                          slot-names)))
                     ;; Evaluate the initform by executing the
                     ;; initfunction.
                     (setf (slot-value-using-class class instance slot)
                           (funcall (slot-definition-initfunction slot)))))))
    ;; Store the class slots in the instance so that we can update the
    ;; instance when the class changes.
    (setf (standard-instance-access instance -1)
          slots))
  instance)
