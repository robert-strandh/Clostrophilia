(cl:in-package #:clostrophilia)

(defun find-method-combination (client name options)
  (let ((template (find-method-combination-template client name)))
    (when (null template)
      (error 'unknown-method-combination
             :name name))
    (ensure-method-combination client template options)))
