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
  (let ((kdefs (make-hash-table :test 'eq)))
    (destructuring-bind (&key entail &allow-other-keys) options
      (loop for spec in arg-specs do
        (when (symbolp spec) (setf spec `(,spec type)))
        (destructuring-bind (name typ) spec
          (setf (gethash name kdefs) typ)))
      (loop for contract in entail
          if (symbolp contract) collect contract into supercontracts
          if (listp contract) collect contract into recursives
          finally
            (return-from def-contract
              `(progn
                 (defclass ,name ,supercontracts ())
                 (defmethod run-contract-enforcement
                     ((o ,name) &key
                      ,@(loop for k being the hash-keys of kdefs collect k))
                   ,@(when supercontracts `((call-next-method)))
                   ,@(loop for contract-call in recursives
                         collect (let ((subcon (car contract-call))
                                       (subargs (cdr contract-call)))
                                   `(run-contract-enforcement ',subcon
                                                              ,@subargs)))
                   ,@(loop for arg-name being the hash-keys of kdefs
                         using (hash-value arg-kind)
                         append
                           (cond
                             ((eq arg-kind 'type)
                              `((unless (find-class ,arg-name)
                                  ,(generate-consequence options
                                                         "~s should be a type"
                                                         arg-name))))
                             (t nil)))
                   ,@(loop for form in forms
                         collect (generate-clause-code (car form) (cdr form)
                                                       kdefs options))
                   t)))))))

