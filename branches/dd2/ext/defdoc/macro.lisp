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

;;;           (setf (get-doc-tags ',name ',spec-type) ',tags)

;;; -----------------------------------------------------------------

(defun decode-defdoc-spec (name-or-spec)
  (cond
   ((symbolp name-or-spec)
    (values name-or-spec (guess-spec-type name-or-spec) nil))
   ((listp name-or-spec)
    (destructuring-bind (spec-type name &rest spec-args) name-or-spec
      (values name spec-type spec-args)))))

;;;(defun decode-defdoc-forms (name spec-type spec-args forms)
;;;  (let (tags
;;;        (descriptive (symbol-name name))
;;;        (result-args nil))
;;;    (flet ((add-spec-args (tag value)
;;;             (push value result-args)
;;;             (push tag result-args))
;;;           (decode-spec-args (args)
;;;             (cond
;;;              ((not (listp args)) args)
;;;              ((eql 1 (length args)) (car args))
;;;              (t `(:paragraphs ,@args)))))
;;;        ;; Unpack the forms
;;;      (loop for form in forms do
;;;        (destructuring-bind (tag . args) form
;;;          (case tag
;;;            ((:descriptive) (setf descriptive (car args)))
;;;            ((:tags) (setf tags args))
;;;            ((:intro :short :full)
;;;             (let ((given (decode-spec-args args)))
;;;               (when (stringp given)
;;;                 (setf given `(:plain ,given)))
;;;               (add-spec-args tag given)))
;;;            ((:params)
;;;             (loop for name-spec in args do
;;;               (let ((this-spec (cadr name-spec)))
;;;                 (when (stringp this-spec)
;;;                   (setf (cadr name-spec) `(:plain ,this-spec)))))
;;;             (add-spec-args tag args))
;;;            ((:deprecated)
;;;             (add-spec-args tag (car args)))
;;;            (otherwise
;;;             (add-spec-args tag args))))))
;;;
;;;    (values `(:spec :self ,name
;;;                    :spec-type ,spec-type
;;;                    :spec-args ,spec-args
;;;                    :descriptive ,descriptive
;;;                    ,@result-args)
;;;            tags)))
