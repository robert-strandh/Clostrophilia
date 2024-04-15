(cl:in-package #:clostrophilia)

(defun ensure-method-combination (client template options)
  (let* ((variant-determiner (variant-signature-determiner template))
         (variant-signature (apply variant-determiner options)))
    (loop for variant in (variants template)
          do (when (equal (variant-signature variant)
                          variant-signature)
               (return-from ensure-method-combination variant)))
    (let ((new-variant (make-instance 'standard-method-combination
                         :variant-signature variant-signature
                         :template template)))
      (push new-variant (variants template))
      new-variant)))
