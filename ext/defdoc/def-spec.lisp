(in-package :defdoc)

;;;(defgeneric defdoc-spec-to (out-type in-type spec-args)
;;;  (:documentation ""))

(defmacro def-spec-format (name-or-name-and-options formals &rest body)
  (declare (ignore name-or-name-and-options formals body))
  nil
;;;  (multiple-value-bind (name no-output)
;;;      (decode-spec-format-name-and-options name-or-name-and-options)
;;;    (declare (ignore no-output))
;;;    (let ((spec (gensym)))
;;;      (loop for form in body
;;;          for cons-p = (consp form)
;;;          for tag = (when cons-p (car form))
;;;          for body = (when cons-p (cdr form))
;;;          if (member tag +formats+)
;;;          collect `(defmethod defdoc-spec-to ((out-type (eql ',tag))
;;;                                              (in-type (eql ',name))
;;;                                              ,spec)
;;;                     (destructuring-bind ,formals (cdr ,spec)
;;;                       (cons ,:out-type (multiple-value-list (progn ,@body)))))
;;;          into methods
;;;          finally
;;;            (return
;;;              `(progn
;;;                 (defmethod defdoc-spec-to ((out-type (eql ',name))
;;;                                            (in-type (eql ',name))
;;;                                            spec)
;;;                   (cons out-type spec))
;;;                 ,@methods)))))
  )

;;;(defun decode-spec-format-name-and-options (name-or-name-and-options)
;;;  (cond
;;;   ((symbolp name-or-name-and-options)
;;;    (values name-or-name-and-options))
;;;   ((listp name-or-name-and-options)
;;;    (destructuring-bind (name &key no-output) name-or-name-and-options
;;;      (values name no-output)))
;;;   (t
;;;    (error "Expected SYMBOL or (SYMBOL &rest ARGS), got ~s"
;;;           name-or-name-and-options))))
