(in-package :defdoc)

;;; -----------------------------------------------------------------
;;; Global hashtable storing information about the kinds of things we
;;; can declare documentation about.

(defclass standard-doc-target ()
     ((name :initarg :name :accessor name)
      (docstring-installer :initarg :docstring-installer
                           :accessor docstring-installer)))

(defvar +doc-target-types+ (make-hash-table :test 'eq)
  "Master global hashtable of all documentation target specifiers.")

(defun get-target-type-docspecs (sym)
  (when (gethash sym +defdocs+)
    (loop for spec being the hash-values of (gethash sym +defdocs+)
          collect spec)))

(defun get-doc-target-types (&optional (sym nil sym-supp-p))
  (cond
   (sym-supp-p
    (loop for type being the hash-keys of +defdocs+ using (hash-value hash)
        if (gethash sym hash) collect type))
   (t
    (loop for type being the hash-keys of +defdocs+ collect type))))

(defun get-target-type (type &optional noerror)
  (let ((type-info (gethash type +doc-target-types+)))
    (unless (or type-info noerror)
      (error "No such documentation type ~s" type))
    type-info))

;;; -----------------------------------------------------------------

(defmacro def-target-type (name (&key (class 'standard-doc-target)) &body forms)
  (multiple-value-bind (lisp-installer lisp-installer-supp-p)
      (decode-doctype-forms forms)
    `(let ((target-spec (make-instance ',class :name ',name)))
       (setf (docstring-installer target-spec)
             #',(cond
                  (lisp-installer-supp-p lisp-installer)
                  (t '(lambda (x y) (declare (ignore x y))))))
       (unless (gethash ',name +defdocs+)
         (setf (gethash ',name +defdocs+) (make-hash-table :test 'eq)))
       (setf (gethash ',name +doc-target-types+)
             target-spec))))

(defun decode-doctype-forms (forms)
  (let ((lisp-installer nil)
        (lisp-installer-supp-p nil))
    (loop for form in forms do
      (case (car form)
        ((:docstring-installer)
         (destructuring-bind ((name spec) &rest body) (cdr form)
           (setf lisp-installer-supp-p t
                 lisp-installer `(lambda (,name ,spec) ,@body))))))
    (values lisp-installer lisp-installer-supp-p)))

;;; -----------------------------------------------------------------

(defun guess-spec-type (name)
  (cond
    ((fboundp name) 'function)
    ((boundp name) 'variable)
    (t (error "Cannot determine name use for documentation of ~s" name))))

;;; -----------------------------------------------------------------
