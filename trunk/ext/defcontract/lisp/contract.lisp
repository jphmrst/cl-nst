;;; File contract.lisp
;;;
;;; This file is part of the DefContract API checker system.
;;;
;;; Copyright (c) 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefContract is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; Defcontract is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Defcontract.  If not, see
;;; <http://www.gnu.org/licenses/>.
(in-package :defcontract)

(defmacro def-contract ((name &rest arg-specs) options &body forms)
  (let ((keyarg-defs (make-hash-table :test 'eq)))
    (loop for spec in arg-specs do
      (when (symbolp spec)
        (setf spec `(,spec type)))
      (destructuring-bind (name typ) spec
        (setf (gethash name keyarg-defs) typ)))
    `(progn
       (defclass ,name () ())
       (defmethod run-contract-enforcement
           ((o ,name) &key ,@(loop for arg being the hash-keys of keyarg-defs
                                   collect arg))
         ,@(loop for arg-name being the hash-keys of keyarg-defs
                 using (hash-value arg-kind)
                 append
                 (cond
                   ((eq arg-kind 'type)
                    `((unless (find-class ,arg-name)
                        ,(generate-consequence options "~s should be a type"
                                               arg-name))))
                   (t nil)))
         ,@(loop for form in forms
               collect (generate-clause-code (car form) (cdr form)
                                keyarg-defs options))
         t))))

