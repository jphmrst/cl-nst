;;; File tag.lisp
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
(in-package :defdoc-core)

(defun get-doc-tags (name type)
  (let ((spec (get-doc-spec name type)))
    (docspec-tags spec)))

(defgeneric format-tag (style package tag stream)
  (:method (style package tag stream)
     (declare (ignore style package))
     ;; (format t "DFT for: ~a~%" tag)
     (format stream "~a" tag)))

(define-condition tag-sort-warning (warning)
    ((tag-sort-warning-style   :initarg :style :reader tag-sort-warning-style)
     (tag-sort-warning-package :initarg :package
                               :reader tag-sort-warning-package)
     (tag-sort-warning-tag     :initarg :tag   :reader tag-sort-warning-tag))
  (:report (lambda (w stream)
             (with-accessors ((style tag-sort-warning-style)
                              (package tag-sort-warning-package)
                              (tag tag-sort-warning-tag)) w
               (pprint-logical-block (stream '(2 3))
                 (format stream "Using default tag-sort 0 for:")
                 (pprint-newline :mandatory stream)
                 (format stream "Using  style ~a" style)
                 (pprint-newline :mandatory stream)
                 (format stream " package ~a" package)
                 (pprint-newline :mandatory stream)
                 (format stream " tag ~a" tag)
                 (pprint-newline :mandatory stream)
                 (princ "Consider using def-doc-tag" stream))))))

(defgeneric tag-sort (style package tag)
  (:method (style package tag)
     (warn 'tag-sort-warning :style style :package package :tag tag)
     0))

(defmacro def-doc-tag (tag (&key (package nil package-supp-p) (style t))
                       &key formatter (sort 0))
  ;; (format t "~s ~s ~s~%" tag package style)
  (let ((spt-params `((style ,style)
                      ,(cond
                        (package-supp-p
                         `(package (eql (find-package ,package))))
                        (t 'package))
                      (tag (eql ',tag)))))
    `(progn
       (defmethod format-tag (,@spt-params stream)
         (funcall #',formatter style package tag stream))
       (defmethod tag-sort (,@spt-params) ,sort))))
