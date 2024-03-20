(cl:in-package #:clostrophilia)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the effective slots of a class.

;;; Implement the behavior of compute-effective-slot-definition for
;;; standard-class and funcallable-standard-class.
;;;
;;; In this function, we use alternative readers for slot-definition
;;; slots.  Thus, we use INITARGS rather than
;;; SLOT-DEFINITION-INITARGS, INITFUNCTION rather than
;;; SLOT-DEFINITION-INITFUNCTION.
;;; Use REAL-CLASS for now.
(defmethod compute-effective-slot-definition
    ((class real-class) name direct-slot-definitions)
  (let ((slot-definition-class (effective-slot-definition-class class)))
    (let (allocation initargs initform initfunction type location)
      (setf allocation
            (slot-definition-allocation (first direct-slot-definitions)))
      ;; When allocation is :CLASS, we use the CONS cell of the
      ;; direct slot definition that determined this allocation as
      ;; the location of the final slot.  If not, the location is
      ;; set to NIL and will be assigned by the appropriate :around
      ;; method.
      (setf location
            (if (eq allocation :class)
                (slot-definition-storage (first direct-slot-definitions))
                nil))
      (setf initargs
            (reduce #'union
                    (mapcar #'slot-definition-initargs direct-slot-definitions)))
      (let ((first-init (find-if-not #'null direct-slot-definitions
                                     :key #'slot-definition-initfunction)))
        (unless (null first-init)
          (setf initform (slot-definition-initform first-init)
                initfunction (slot-definition-initfunction first-init))))
      (setf type
            `(and ,@(mapcar #'slot-definition-type direct-slot-definitions)))
      (if (null initfunction)
          (make-instance slot-definition-class
            :name name
            :allocation allocation
            :location location
            :initargs initargs
            :type type)
          (make-instance slot-definition-class
            :name name
            :allocation allocation
            :location location
            :initargs initargs
            :initform initform
            :initfunction initfunction
            :type type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default action for the primary method on COMPUTE-SLOTS.
;;;
;;; The AMOP says that this function returns the resulting list of
;;; slots in an "unspecified order".  Analyzing the text of the AMOP a
;;; bit more, we understand that the order can be specified by the
;;; implementation, that this order determines the LOCATION of each
;;; directly accessible slot in that the default :AROUND method on
;;; COMPUTE-SLOTS allocates locations with increasing locations
;;; according to this order.
;;;
;;; Here, we want the slots to appear in an instance with slots
;;; defined in more general classes first, so that is what this
;;; function does.
;;;
;;; The AMOP says that lists of direct slot definitions are grouped by
;;; name into separate lists, and that each list is then sorted
;;; according to the class precedence list.  However, we can not use
;;; CLASS-PRECEDENCE-LIST to access the precedence list, because the
;;; AMOP also stipulates that this function signals an error if the
;;; class is not finalized, and computing the effective slots is part
;;; of the class finalization protocol.  For that reason, we use the
;;; alternative reader PRECEDENCE-LIST that works just like the normal
;;; CLASS-PRECEDENCE-LIST, except that it just accesses the slot, and
;;; does not signal an error if the class has not been finalized.
;;;
;;; The AMOP does not say what is supposed to happen if this generic
;;; function is called if the class precedence list has not first been
;;; computed and made available.  Here (at least for now), we make the
;;; assumption that the application programmer is not supposed to call
;;; this function directly, so that it is only called from
;;; FINALIZE-INHERITANCE, and that the application programmer is only
;;; allowed to write methods on it.

;;; FIXME: We are probably not doing this quite right.  We should
;;; probably use the class precedence list directly to sort the direct
;;; slot definitions within a list with the same slot name.
;;;
;;;
;;; Use REAL-CLASS for now.
(defmethod compute-slots ((class real-class))
  (let* (;; Do NOT call CLASS-PRECEDENCE-LIST here (see comment
         ;; above).  Instead use alternative reader PRECEDENCE-LIST.
         (superclasses (precedence-list class))
         ;; We can't use CLASS-DIRECT-SLOTS here, because according to
         ;; the AMOP it must return the empty list for built-in
         ;; classes, and we need to inherit slots from built-in
         ;; classes as well.  For that reason, we use a different
         ;; reader named DIRECT-SLOTS which does the same thing as
         ;; CLASS-DIRECT-SLOTS except that for built-in classes, it
         ;; retuns the direct slots of the built-in class.
         (direct-slots (mapcar #'class-direct-slots superclasses))
         (concatenated (reduce #'append direct-slots))
         (reverse-slots (reverse direct-slots))
         (reverse-concatenated (reduce #'append reverse-slots))
         (names (mapcar #'slot-definition-name reverse-concatenated))
         (unique (remove-duplicates names :from-end t)))
    (loop for name in unique
          collect (compute-effective-slot-definition
                   class
                   name
                   (remove name concatenated
                           :key #'slot-definition-name
                           :test-not #'eq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default action for :AROUND method on COMPUTE-SLOTS.
;;;
;;; The AMOP specifies that:
;;;
;;;    "For a given class, the locations increase consecutively, in
;;;     the order that the directly accessible slots appear in the
;;;     list of effective slots."
;;;
;;; It is not entirely clear what list of effective slots is meant,
;;; but we take this to mean that it is the list of effective slots
;;; that is both returned by the primary method and associated with
;;; the class.

;;; Use REAL-CLASS for now.
(defmethod compute-slots :around ((class real-class))
  (let ((slots (call-next-method))
        (next-location 0))
    (loop for slot in slots
          do (when (eq (slot-definition-allocation slot) :instance)
               (setf (slot-definition-location slot)
                     next-location)
               (incf next-location)))
    slots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-DEFAULT-INITARGS.
;;;
;;; The AMOP does not say what is supposed to happen if this generic
;;; function is called if the class precedence list has not first been
;;; computed and made available.  Here (at least for now), we make the
;;; assumption that the application programmer is not supposed to call
;;; this function directly, so that it is only called from
;;; FINALIZE-INHERITANCE, and that the application programmer is only
;;; allowed to write methods on it.

(defun compute-default-initargs-default (class)
  (remove-duplicates
   (reduce #'append
           ;; We use the reader DIRECT-DEFAULT-INITARGS rather than
           ;; CLASS-DIRECT-DEFAULT-INITARGS so that this function
           ;; works for built-in classes as well as standard classes.
           ;;
           ;; Furthermore, we use the alternative reader named
           ;; PRECEDENCE-LIST rather than CLASS-PRECEDENCE-LIST,
           ;; because the latter signals an error if the class is not
           ;; finalized.
           (mapcar #'class-direct-default-initargs
                   (precedence-list class)))
   :test #'eq
   :key #'car
   :from-end t))
