;;; File doc.lisp
;;;
;;; This file is part of the DefContract documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefContract is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; DefContract is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with DefContract.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.
(in-package :defcontract-doc)

(def-documentation (compiler-macro def-contract)
  (:intro "The "
          (:lisp compiler-macro def-contract)
          " macro names a new contract and enumerates what that contract should enforce.")
  (:callspec ((name (:bag param (param kind)))
              (&key (entail ((:bag contract-name
                                   (contract-name (:seq params))))))
              &body (:seq clause)))
  (:params (name "")
           (param "")
           (kind "")
           (contract-name "")
           (params "")
           (clause "")))

(def-documentation (compiler-macro apply-contract)
  (:intro "The "
          (:lisp compiler-macro apply-contract)
          " macro checks adherence to a contract.")
  (:callspec (contract-name (:seq keyword-arguments)))
  (:params (contract-name "")
           (contract-arguments "")))

(def-target-type contract-clause (:lower-case "contract clause"
                                  :symbol-definition-nocheck t))

(def-documentation (contract-clause has-method)
  (:intro "The "
          (:lisp compiler-macro has-method)
          " clause checks that a method definition for a generic function has been defined for particular classes of arguments.")
  (:callspec (generic-function-name (:seq argument-spec) result-type-spec))
  (:params (generic-function-name "")
           (argument-spec "")
           (result-type-spec "")))

(ensure-api-documentation :defcontract)

(def-output-class (defcontract-manual
                      :title "DefContract user manual" :author "John Maraist"
                      :leader (:seq "DefContract is a simple framework for recording API contracts and other desired properties of Common Lisp programs, and verifying them at load time.  The system defines two macros "
          (:lisp compiler-macro def-contract)
          " and "
          (:lisp compiler-macro apply-contract)
          " intended for top-level use, which respectively define the general terms of a contract, and enforce it for a particular case.  The body of a call to "
          (:lisp compiler-macro def-contract)
          " includes "
          (:emph "contract clauses")
          " which enumerate the contract's enforceable assertions."))
    (collect-symbols :defcontract #:def-contract)
    (collect-target-type 'contract-clause)
    (collect-symbols :defcontract #:apply-contract))

(defclass manual-style (symbol-homing-style
                        latex-style
                        docspec-fancy-header-latex-style) ())

(defmethod candidate-home-packages ((style manual-style) target-type spec)
  (declare (ignore target-type spec))
  '(:defcontract))

