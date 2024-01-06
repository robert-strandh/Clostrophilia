(cl:in-package #:clostrophilia)

;;; For a given class, compute a precedence relation.
;;; This relation is represented as a list of cons cells,
;;; where the class in the CAR of the cell takes precedence over
;;; the class in the CDR of the cell. For a class C with a list of
;;; direct superclasses (S1 S2 ... Sn), the relation contains
;;; the elements (C . S1) (S1 . S2) ... (Sn-1 . Sn).
(defun compute-relation (class)
  (loop for prev = class then super
        for super in (class-direct-superclasses class)
        collect (cons prev super)))

(defun compute-class-precedence-list-assuming-superclasses-finalized (class)
  (let* ((all-supers (cons class
                           (remove-duplicates
                            (reduce #'append
                                    (mapcar #'class-precedence-list
                                            (class-direct-superclasses class))))))
         (relation (loop for class in all-supers
                         append (compute-relation class)))
         (reverse-result '()))
    (flet ((find-candidate ()
             (let ((candidates
                     ;; Start by looking for all remaining classes that
                     ;; depend on no other class according to the relation.
                     (loop for super in all-supers
                           unless (find super relation :key #'cdr)
                             collect super)))
               ;; If no such class exists, we have a circular dependence,
               ;; and so we can't compute the class precedence list.
               (when (null candidates)
                 ;; FIXME: do this better
                 (error 'unable-to-compute-class-precedence-list
                        :offending-class class))
               (if (null (cdr candidates))
                   ;; A unique candiate, return it.
                   (car candidates)
                   ;; Several candidates.  Look for the last class in the
                   ;; result computed so far (first in the reversed result)
                   ;; that has one of the candidates as a direct superclass.
                   ;; Return that candidate.
                   ;; There can be at most one, because within a list of
                   ;; superclasses, there is already a dependency, so that
                   ;; two different classes in a single list of superclasses
                   ;; can not both be candidates.
                   ;; There must be at least one, because ...
                   ;; (FIXME: prove it!)
                   (loop for element in reverse-result
                         do (loop for candidate in candidates
                                  do (when (member candidate
                                                   (class-direct-superclasses
                                                    element))
                                       (return-from find-candidate
                                         candidate))))))))
      (loop until (null all-supers)
            do (let ((candidate (find-candidate)))
                 (push candidate reverse-result)
                 (setf all-supers (remove candidate all-supers))
                 (setf relation (remove candidate relation :key #'car)))))
    (reverse reverse-result)))

(defmethod compute-class-precedence-list ((class class))
  ;; Make sure all the direct superclasses are already finalized so
  ;; that we can use their precedence lists.
  (loop for super in (class-direct-superclasses class)
        do (unless (class-finalized-p super)
             (finalize-inheritance super)))
  (compute-class-precedence-list-assuming-superclasses-finalized class))
