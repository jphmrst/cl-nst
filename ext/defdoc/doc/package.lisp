;;; File package.lisp
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
(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (intern (symbol-name '#:asdf-defdoc) (find-package :defdoc)))

(defpackage :sift.defdoc-doc
  (:documentation "Unit and regression testing for Common Lisp")
  (:nicknames :defdoc-doc)
  (:use :common-lisp :defdoc :defdoc-control-api)
  #+(or sbcl allegro)
  (:import-from #+sbcl sb-mop #-sbcl mop
                #:generic-function-methods #:method-specializers
                #:eql-specializer-object)
  #+(or openmcl clozure)
  (:import-from ccl
    #:extract-lambda-list
    #:generic-function-methods #:method-specializers
    #:eql-specializer-object)

  (:import-from defdoc
    #:manual-section #:docspecs #:outspec #:asdf-defdoc ; #:doc-gen
    #:control #:targets #:model
    #:label-model #:elements #:standard-model #:output-model
    #| #:plaintext |# #:plaintext-model
    #:styles #:latex #:latex-style-model
    #:deprecated)

  (:export #:build-defdoc-docs))
