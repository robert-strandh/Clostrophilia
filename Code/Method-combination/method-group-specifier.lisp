(cl:in-package #:clostrophilia)

(defun make-qualifier-test (qualifier-pattern qualifiers-var)
  (cond ((eq qualifier-pattern '*)
         t)
        ((listp qualifier-pattern)
         (let ((last (last qualifier-pattern))
               (butlast (butlast qualifier-pattern)))
           (cond ((null (cdr last))
                  `(equal ,qualifiers-var ',qualifier-pattern))
                 ((eq (cdr last) '*)
                  (let* ((prefix (append butlast (list (car last))))
                         (length (length prefix)))
                    `(equal (subseq ,qualifiers-var 0 ,length)
                            ',prefix)))
                 (t
                  (error "A dotted list for qualifier pattern must end with *")))))
        (t
         (error "A qualifier pattern must be either a list or the symbol *"))))

(defun parse-qualifier-patterns (qualifier-patterns qualifiers-var)
  `(or ,@(loop for pattern in qualifier-patterns
               collect (make-qualifier-test pattern qualifiers-var))))

(defun parse-method-group-specifier (method-group-specifier qualifiers-var)
  (cond ((not (ecclesia:proper-list-p method-group-specifier))
         (error 'method-group-specifier-must-be-proper-list
                :group-specifier method-group-specifier))
        ((not (symbolp (first method-group-specifier)))
         (error "name of method group specifier must be a symbol"))
        ((null (rest method-group-specifier))
         (error "method group specifier must have a predicate or qualifier pattern(s)"))
        (t
         (let* ((option-names '(:description :order :required))
                (position (position-if (lambda (x) (member x option-names))
                                       method-group-specifier
                                       :start 2))
                (options (if (null position)
                             '()
                             (subseq method-group-specifier position)))
                (remaining (subseq method-group-specifier 0 position))
                (second (second remaining)))
           ;; FIXME: check the options for validity
           (list (first remaining)
                 (if (and (= (length remaining) 2)
                          (symbolp second)
                          (not (null second))
                          (not (eq second '*)))
                     `(,second qualifiers)
                     (parse-qualifier-patterns (rest remaining) qualifiers-var))
                 options)))))
