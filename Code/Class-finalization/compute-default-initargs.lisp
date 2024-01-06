(cl:in-package #:clostrophilila)

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

;;; Use REAL-CLASS for now.
(defmethod compute-default-initargs ((class real-class))
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
