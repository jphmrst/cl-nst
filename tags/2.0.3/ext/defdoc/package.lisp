;;; File package.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
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
(in-package :common-lisp-user)

(defpackage :def-doc
    (:documentation "Structured documentation definition")
    (:nicknames :ddoc)
;;;    (:use :closer-common-lisp)
;;;    #+(or sbcl allegro)
;;;    (:import-from #+sbcl sb-mop #-sbcl mop
;;;                  #:generic-function-methods #:method-specializers
;;;                  #:eql-specializer-object)
;;;    #+(or openmcl clozure)
;;;    (:import-from ccl
;;;                  #:extract-lambda-list
;;;                  #:generic-function-methods #:method-specializers
;;;                  #:eql-specializer-object)

    (:export #:def-doc))

(defun nst::make-package-documentation ()
  "Write documentation for this package, using system package-doc."
  (asdf:oos 'asdf:load-op 'package-doc)
  (funcall (symbol-function (intern (symbol-name 'package-doc)
                                    (find-package :package-doc)))
           (find-package :def-doc)))