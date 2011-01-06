;;; File storage.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; DefDoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with DefDoc.  If not, see
;;; <http://www.gnu.org/licenses/>.

(in-package :defdoc)

(defvar +defdocs+ (make-hash-table :test 'eq)
  "Master global hashtable of all documentation specifiers.")

(defun get-doc-hash-of-target-type (type)
  (let ((type-hash (gethash type +defdocs+)))
    (unless type-hash
      (error "No such documentation type ~s" type))
    type-hash))

(defun get-doc-spec (name type)
  (let ((type-hash (get-doc-hash-of-target-type type)))
    (gethash name type-hash)))

(defun get-doc-specs (name)
  (loop for hash being the hash-values of +defdocs+
        for spec = (gethash name hash)
        if spec collect spec))

(define-setf-expander get-doc-spec (name type)
  (let ((store (gensym))
        (type-hash (gensym)))
    (values nil
            nil
            `(,store)
            `(let ((,type-hash (get-doc-hash-of-target-type ,type)))
               (setf (gethash ,name ,type-hash) ,store))
            `(get-doc-spec ,name ,type))))

(defun all-by-defdocs-storage-collector ()
  (loop for subhash being the hash-values of +defdocs+
        append (loop for spec being the hash-values of subhash
                     collect spec)))

(setf (gethash #'all-by-defdocs-storage-collector +all-specs-collectors+) t)

(defmacro with-name-and-filters ((name-var filters-var form) &body body)
  (let ((form-value (gensym)))
    `(let ((,form-value ,form)
           ,name-var ,filters-var)
       (cond
         ((listp ,form-value)
          (setf ,name-var (car ,form-value) ,filters-var (cdr ,form-value)))
         ((symbolp ,form-value)
          (setf ,name-var ,form-value ,filters-var nil))
         (t
          (error "Expected symbol or list, got ~s" ,form-value)))
       ,@body)))

(defun get-filter-predicate (filter-keyargs)
  (destructuring-bind (&key (package nil package-supp-p)) filter-keyargs
    #'(lambda (spec)
        (block filter-block
          (when package-supp-p
            (unless (eq (find-package package)
                        (symbol-package (docspec-self spec)))
              (return-from filter-block nil)))
          t))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun get-specs-by-target-type (target-name &rest filters)
  (loop for spec being the hash-values of (gethash target-name +defdocs+)
        for filter in filters
        if (funcall filter spec) collect spec))

;;;  Hope to get rid of this after new output-framework spec.
;;;
(defun get-target-types-collector (target-type-specs)
  #'(lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for target-type-spec in target-type-specs do
        (with-name-and-filters (type filters target-type-spec)
          (let* ((specs (get-specs-by-target-type
                                          type (get-filter-predicate filters))))
            (loop for spec in specs do
              (setf (gethash spec result) t)))))
      result))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun get-specs-by-package-export (package-name filter &optional warn-if-undoc)
  (let ((result nil))
    (do-external-symbols (s (find-package package-name))
      (let ((symbol-used nil))
        (loop for target-type-hash being the hash-values of +defdocs+
              for spec = (gethash s target-type-hash)
              if (and spec (funcall filter spec))
                do (push spec result)
                   (setf symbol-used t))
        (when (and warn-if-undoc (not symbol-used))
          (warn "No specification for ~s" s))))
    (nreverse result)))

;;;  Hope to get rid of this after new output-framework spec.
;;;
(defun get-packages-exported-collector (package-names
                                        &optional (warn-if-undoc t))
  #'(lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for package-name-spec in package-names do
        (with-name-and-filters (package-name raw-filters package-name-spec)
          (loop for spec in (get-specs-by-package-export
                             package-name (get-filter-predicate raw-filters)
                                warn-if-undoc)
              do (setf (gethash spec result) t))))
      result))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun get-specs-by-package-allsymbols (package-name filter warn-if-undoc)
  (let ((result nil))
    (do-symbols (s (find-package package-name))
      (let ((symbol-used nil))
        (loop for target-type-hash being the hash-values of +defdocs+
              for spec = (gethash s target-type-hash)
              if (and spec (funcall filter spec))
                do (push spec result)
                   (setf symbol-used t))
        (when (and warn-if-undoc (not symbol-used))
          (warn "No specification for ~s" s))))
    (nreverse result)))

;;;  Hope to get rid of this after new output-framework spec.
;;;
(defun get-packages-symbols-collector (package-names &optional warn-if-undoc)
  #'(lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for package-name-spec in package-names do
        (with-name-and-filters (package-name raw-filters package-name-spec)
          (loop for spec
                in (get-specs-by-package-allsymbols package-name
                                                    (get-filter-predicate
                                                        raw-filters)
                       warn-if-undoc)
                do (setf (gethash spec result) t))))
      result))
