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

(defmethod generate-clause-code (tag x y z)
  (declare (ignore x y z))
  (error "Unknown contract clause ~s" tag))

(defmethod generate-clause-code ((tag (eql 'has-method))
                                 clause-args keyarg-defs contract-opts)
  (destructuring-bind (method-spec) clause-args
    (destructuring-bind (method-name method-arg-spec result-type-spec)
        method-spec
      (declare (ignore result-type-spec))
      (loop for spec in method-arg-spec
            if (eq spec t)
              collect '(find-class t) into actual-args-specs
              and collect t into display-arg-specs
            else if (gethash spec keyarg-defs)
                   collect `(find-class ,spec) into actual-args-specs
                   and collect spec into display-arg-specs
            else if (symbolp spec)
                   collect `(find-class ',spec) into actual-args-specs
                   and collect `',spec into display-arg-specs
            else do (error "Got ~s, expect symbol" spec) end
            finally
         (return-from generate-clause-code
           `(unless (#-clozure-common-lisp
                     closer-mop:compute-applicable-methods-using-classes
                     #+clozure-common-lisp
                     compute-applicable-methods-using-classes

                     (symbol-function ',method-name) (list ,@actual-args-specs))
              ,(generate-consequence contract-opts "No method ~s ~s"
                                     `',method-name
                                     `(list ,@display-arg-specs))))))))

(defmethod generate-clause-code ((tag (eql 'is-subtype-of))
                                 clause-args keyarg-defs contract-opts)
  (declare (ignore keyarg-defs contract-opts))
  (loop for (subtype supertype) in clause-args
        collect `(warn "Not checking subtypes: ~s of ~s" ',subtype ',supertype)
          into subtype-checks
        finally (return-from generate-clause-code
                  `(progn ,@subtype-checks))))

(defun generate-consequence (options string-or-class &rest args)
  (declare (ignore options))
  `(warn ,string-or-class ,@args))

(defgeneric run-contract-enforcement (contract &key &allow-other-keys)
  (:method ((contract symbol) &rest keyvals)
    (apply #'run-contract-enforcement (make-instance contract) keyvals)))