(in-package :defdoc)

(defgeneric defdoc-spec-to (out-type in-type spec-args)
  (:documentation ""))

(defmacro def-spec-format (name-or-name-and-options formals &rest body)
  (multiple-value-bind (name)
      (decode-spec-format-name-and-options name-or-name-and-options)
    (let ((spec (gensym)))
      (loop for form in body
          for cons-p = (consp form)
          for tag = (when cons-p (car form))
          for body = (when cons-p (cdr form))
          if (member tag +formats+)
          collect `(defmethod defdoc-spec-to ((out-type (eql ',tag))
                                              (in-type (eql ',name))
                                              ,spec)
                     (destructuring-bind ,formals (cdr ,spec)
                       ,@body))
          into methods
          finally
            (return
              `(progn
                 (defmethod defdoc-spec-to ((out-type (eql ',name))
                                            (in-type (eql ',name))
                                            spec)
                   spec)
                 ,@methods))))))

(defun decode-spec-format-name-and-options (name-or-name-and-options)
  (cond
   ((symbolp name-or-name-and-options)
    (values name-or-name-and-options))
   ((listp name-or-name-and-options)
    (destructuring-bind (name &key) name-or-name-and-options
      (values name)))
   (t
    (error "Expected SYMBOL or (SYMBOL &rest ARGS), got ~s"
           name-or-name-and-options))))
