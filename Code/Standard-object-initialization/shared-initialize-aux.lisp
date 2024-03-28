(cl:in-package #:clostrophilia)

;;; We assume that this code is executed to create function
;;; shared-initialize-aux with refinement R.  INSTANCE is an object
;;; with refinement R+2.  CLASS is the class of the instance, so it is
;;; an object of refinement R+1.  SLOT-NAMES is the same as the
;;; argument to SHARED-INITIALIZE.

(defun shared-initialize-aux (instance class slot-names &rest initargs)
  (let ((slots (class-slots class)))
    (loop for slot in slots
          do ;; (multiple-value-bind (key value foundp)
             ;;     ;; Find the first key/value pair in initargs where
             ;;     ;; the key is one of the initargs of the slot.
             ;;     (get-properties initargs (slot-definition-initargs slot))
             ;;   (declare (ignore key))
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
