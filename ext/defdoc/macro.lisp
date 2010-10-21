(in-package :defdoc)

(defmacro def-documentation (name-or-spec &body body)
  "Doc doc doc"
  (multiple-value-bind (name target-type spec-args)
      (decode-defdoc-spec name-or-spec)
    (let ((spec (gensym "spec")))
      `(let ((,spec (compile-spec ',name ',target-type ',spec-args ',body)))
         (setf (get-doc-spec ',name ',target-type) ,spec)
         (funcall (docstring-installer (get-target-type ',target-type))
                  ',name ,spec)))))

(defun decode-defdoc-spec (name-or-spec)
  (cond
   ((symbolp name-or-spec)
    (values name-or-spec (guess-spec-type name-or-spec) nil))
   ((listp name-or-spec)
    (destructuring-bind (spec-type name &rest spec-args) name-or-spec
      (values name spec-type spec-args)))))

;;; -----------------------------------------------------------------
