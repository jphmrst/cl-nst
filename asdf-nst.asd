;;; File asdf-nst.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2009-2011 Smart Information Flow Technologies.
;;; Copyright (c) 2015-2016 John Maraist
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.

(defpackage :asdf-nst-asd (:use :common-lisp :asdf))
(in-package :asdf-nst-asd)

(defsystem :asdf-nst
    :description "NST integration with ASDF"
    :version "1.0.0"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :components ((:module "asdf" :components
                          (;; The ASDF-NST package.
                           (:file "package")

                           ;; Dynamic variables and generic functions.
                           (:file "globals"  :depends-on ("package"))

                           ;; An NST-testable ASDF class
                           (:file "runnable"  :depends-on ("globals"))

                           ;; An NST-testable ASDF class
                           (:file "system"  :depends-on ("runnable"))

                                        ; See also: misc.lisp has the
                                        ; beginnings of a :nst-file
                                        ; type on which we should
                                        ; allow deferred
                                        ; compilation/loading until
                                        ; test time.

                           ))))
