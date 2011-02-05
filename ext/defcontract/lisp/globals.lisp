;;; File globals.lisp
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
;;; DefContract is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with DefContract.  If not, see
;;; <http://www.gnu.org/licenses/>.
(in-package :defcontract)

#-allegro
(defmacro named-function (name lambda-expression)
  (declare (ignore name))
  `(function ,lambda-expression))

(defgeneric generate-clause-code (tag clause-args keyarg-defs contract-opts))

(defmethod generate-clause-code ((tag (eql 'has-method))
                                 clause-args keyarg-defs contract-opts)
  (destructuring-bind (method-spec) clause-args
    (destructuring-bind (method-name method-arg-spec result-type-spec)
        method-spec
      (declare (ignore result-type-spec))
      (let ((actual-args-specs (loop for spec in method-arg-spec
                                   collect
                                     (cond
                                       ((eq spec t) '(find-class t))
                                       ((gethash spec keyarg-defs)
                                        `(find-class ,spec))
                                       ((symbolp spec) `(find-class ',spec))
                                       (t
                                        (error "Got ~s, expect symbol" spec))))))
        `(unless (#-clozure-common-lisp closer-mop:compute-applicable-methods-using-classes
                  #+clozure-common-lisp compute-applicable-methods-using-classes
                  (symbol-function ',method-name) (list ,@actual-args-specs))
           ,(generate-consequence contract-opts "No method ~s ~s"
                                  `',method-name `',method-arg-spec))))))

(defun generate-consequence (options string-or-class &rest args)
  (declare (ignore options))
  `(warn ,string-or-class ,@args))