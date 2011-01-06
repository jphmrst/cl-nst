;;; File select.lisp
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

(defmacro with-conjunctive-options ((arg options) item &body body)
  (let ((item-var (gensym)))
    `(let* ((,item-var ,item)
            (,arg (cond
                   ((symbolp ,item-var) ,item-var)
                   ((listp ,item-var)   (car ,item-var))
                   (t (error "Expected symbol or list, got ~s" ,item-var))))
            (,options (cond
                       ((symbolp ,item-var) nil)
                       ((listp ,item-var)   (cdr ,item-var)))))
      ,@body)))

(defun get-wrapped-core (core variable variable-supp-p)
  (cond
   (variable-supp-p (case (length core)
                      ((0) nil)
                      ((1) (car core))
                      (otherwise `(progn ,@core))))
   (t `#'(lambda (,variable) ,@core))))

(defun compile-filter-spec (spec spec-var)
  (declare (ignore spec-var))
  (case (car spec)
    (otherwise
     (error "Unrecognized spec filter head ~s" (car spec)))))

(defun compile-package-symbol-generator (macroname spec-args warn-undoc
                                                   results-hash)
  (let* ((sym (gensym)) (names (gensym)) (name (gensym))
         (spec (gensym)) (specs (gensym)))
    (loop for package-spec in spec-args
          collect
      (with-conjunctive-options (package filter-specs) package-spec
        (let ((filters (loop for filter-spec in filter-specs
                           collect (compile-filter-spec filter-spec spec)))
              (core `(setf (gethash ,spec ,results-hash) t)))
          (loop for filter in filters do
            (setf core `(when ,filter ,core)))
          `(loop for ,spec
                 in (let ((,names nil))
                      (,macroname (,sym (find-package ',package))
                                  (push ,sym ,names))
                      (loop for ,name in ,names
                          append
                            ,(let ((specs-expr `(get-doc-specs ,name)))
                               (cond
                                (warn-undoc
                                 `(let ((,specs ,specs-expr))
                                    (when (not ,specs)
                                      (warn "Undocumented: ~s" ,name))
                                    ,specs))
                                (t specs-expr)))))
               do ,core))))))

(defun compile-docspec-gatherer (spec &optional (results nil results-supp-p))
  (let ((spec-hd (car spec))
        (spec-tl (cdr spec)))
    (get-wrapped-core
     (case spec-hd
       ((:exported-symbols)
        (compile-package-symbol-generator 'do-external-symbols
                                         spec-tl t results))
       ((:all-symbols)
        (compile-package-symbol-generator 'do-symbols spec-tl t results))
       ((:documented-symbols)
        (compile-package-symbol-generator 'do-symbols spec-tl nil results))
       ((:target-type)
        `(loop for target-type-spec in ,spec-tl do
               (with-conjunctive-options (target-type filters)
                   target-type-spec
                 (loop for spec being the hash-values
                     of (gethash target-type +defdocs+)
                     if (check-spec-on-filters spec filters)
                     do (setf (gethash spec results) t)))))
        ((:with-sets)
         (error "docspec selector ~s not currently supported" :with-sets))
        (otherwise
         (error "unknown docspec selector ~s" spec-hd)))
     results results-supp-p)))

(defun select-docspecs (criteria)
  (let ((results (make-hash-table :test 'eq)))
    (eval `(compile-docspec-gatherer ',criteria results))
    (loop for spec being the hash-keys of results collect spec)))
