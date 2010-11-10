;;; File collectable.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with NST.  If not, see
;;; <http://www.gnu.org/licenses/>.
(in-package :defdoc)

(defvar +all-specs-collectors+ (make-hash-table :test 'eq))
(defun all-specs-collector (&optional (result (make-hash-table :test 'eq)))
  (loop for c being the hash-keys of +all-specs-collectors+
        append (funcall c result))
  result)

(defun filter-collector (collector filter)
  #'(lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for spec being the hash-keys of (funcall collector)
            if (funcall filter spec)
              do (setf (gethash spec result) t))
      result))

(defmacro collector-unitef (base-collection new-collection)
  (let ((prev (gensym))
        (result (gensym)))
    `(let ((,prev ,base-collection))
       (setf ,base-collection
         #'(lambda (&optional (,result (make-hash-table :test 'eq)))
             (funcall ,prev ,result)
             (funcall ,new-collection ,result)
             ,result)))))

(defmacro collectors-unitef (base-collection new-collections)
  (let ((prev (gensym))
        (result (gensym)))
    `(let ((,prev ,base-collection))
       (setf ,base-collection
             #'(lambda (&optional (,result (make-hash-table :test 'eq)))
                 (funcall ,prev ,result)
                 (loop for c in ,new-collections do (funcall c ,result))
                 ,result)))))
