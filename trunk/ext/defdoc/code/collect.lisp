;;; File collectable.lisp
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

(defgeneric label-values (labeled-entity))

(defvar +all-specs-collectors+ (make-hash-table :test 'eq))
(defun all-specs-collector (&optional (result (make-hash-table :test 'eq)))
  (loop for c being the hash-keys of +all-specs-collectors+
        append (funcall c result))
  result)

(defun filter-collector (collector filter)
  (named-function filter-collector-thunk
    (lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for spec being the hash-keys of (funcall collector)
          if (funcall filter spec)
          do (setf (gethash spec result) t))
      result)))

(defmacro collector-unitef (base-collection new-collection-form)
  (let ((prev (gensym))
        (addl (gensym))
        (result (gensym)))
    `(let ((,prev ,base-collection)
           (,addl ,new-collection-form))
       (setf ,base-collection
        (named-function collector-unitef-thunk
          (lambda (&optional (,result (make-hash-table :test 'eq)))
            (funcall ,prev ,result)
            (funcall ,addl ,result)
            ,result))))))

(defmacro collectors-unitef (base-collection new-collections)
  (let ((prev (gensym))
        (result (gensym)))
    `(let ((,prev ,base-collection))
       (setf ,base-collection
        (named-function collectors-unitef-thunk
          (lambda (&optional (,result (make-hash-table :test 'eq)))
            (funcall ,prev ,result)
            (loop for c in ,new-collections do (funcall c ,result))
            ,result))))))
