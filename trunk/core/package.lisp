;;; File package.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
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

(defpackage :sift.nst
    (:documentation "Unit and regression testing for Common Lisp")
    (:nicknames :nst)
    (:use :closer-common-lisp)
    #+(or sbcl allegro)
    (:import-from #+sbcl sb-mop #-sbcl mop
                  #:generic-function-methods #:method-specializers
                  #:eql-specializer-object)
    #+(or openmcl clozure)
    (:import-from ccl
                  #:extract-lambda-list
                  #:generic-function-methods #:method-specializers
                  #:eql-specializer-object)

    ;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    ;; * REMEMBER! * REMEMBER! * REMEMBER! * REMEMBER! * REMEMBER! * *
    ;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    ;; * * * Mirror and document the exported API in the manual. * * *
    ;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    (:export #:def-fixtures
             #:def-test-group
             #:def-test #:def-check     ; def-check will be deprecated
                                        ; and then dropped

             #:def-criterion-alias  #:def-check-alias
             #:def-values-criterion #:def-value-check
             #:def-form-criterion   #:def-control-check
                                        ; def-check-alias,
                                        ; def-value-check and
                                        ; def-control-check are
                                        ; deprecated and will be
                                        ; dropped.

             ;; Do NOT change this package's exported-symbol list
             ;; without updating the API in the manual (or filing a
             ;; ticket for the addition).

             #:continue-check

             #:emit-failure #:emit-warning #:emit-success
             #:add-failure #:add-error #:add-info
             #:check-result             ; check-result is deprecated
                                        ; and will be dropped.

             ;; Do NOT change this package's exported-symbol list
             ;; without updating the API in the manual (or filing a
             ;; ticket for the addition).

             #:run-package #:run-group #:run-test
             #:report-multiple #:report-package #:report-group #:report-test
             #:nst-cmd

             #:nst-junit-dump
             #:junit-results-by-group

             ;; Do NOT change this package's exported-symbol list
             ;; without updating the API in the manual (or filing a
             ;; ticket for the addition).

             #:*nst-output-stream*
             #:*default-report-verbosity*
             #:*debug-on-error*

             #:arbitrary
             #:compound-structure
             #:def-arbitrary-instance-type

             ;; Do NOT change this package's exported-symbol list
             ;; without updating the API in the manual (or filing a
             ;; ticket for the addition).

             ;; If ever we break up the NST package, this name should
             ;; go into the programmer's API package.
             #:protect-nst-config
             #:apply-debug-options)

    ;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    ;; * REMEMBER! * REMEMBER! * REMEMBER! * REMEMBER! * REMEMBER! * *
    ;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    ;; * * * Mirror and document the exported API in the manual. * * *
    ;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    )

(defun nst::make-package-documentation ()
  "Write documentation for this package, using system package-doc."
  (asdf:oos 'asdf:load-op 'package-doc)
  (funcall (symbol-function (intern (symbol-name 'package-doc)
                                    (find-package :package-doc)))
           (find-package :nst)))
