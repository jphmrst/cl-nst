(in-package :defdoc)

(defvar +label-defs+ (make-hash-table :test 'eq))

(defgeneric get-compiled-labeldef (package name options forms))
(defvar *label-compiler* #'get-compiled-labeldef)

(defmacro def-label (name options &body body)
  (let ((p (gensym))
        (spec (gensym)))
    `(let* ((,p (symbol-package ',name))
            (,spec (funcall *label-compiler* ,p ',name ',options ',body)))
        (setf (gethash ',name +label-defs+) ,spec)
        ',name)))

;;; -----------------------------------------------------------------

(defvar *label-class* 'standard-doc-label)
(defgeneric get-label-class (package name forms)
  (:method (package name forms)
     (declare (ignore package name forms))
     *label-class*))

(defclass doc-label () ())

(defclass standard-doc-label (doc-label) ())

(defmethod get-compiled-labeldef (package name options forms)
  (let* ((use-class (get-label-class package name forms))
         (result (make-instance use-class
                   :self name :target-type target-type)))
    (loop for form in forms do
          (let ((hd (car form))
                (tl (cdr form)))
            (process-standard-labeldef-form package target-type
                                            hd name tl result)))
    result))

(defun compile-label (name target-type label-args forms)
  (declare (ignore label-args))
  (get-compiled-labeldef (symbol-package name) target-type name forms))

(defgeneric process-standard-labeldef-form (package target-type form-head
                                           target-name form-args labeldef)
  (:method (package target-type form-head target-name form-args labeldef)
     (declare (ignore package target-type
                      form-head target-name form-args labeldef))
     nil))

