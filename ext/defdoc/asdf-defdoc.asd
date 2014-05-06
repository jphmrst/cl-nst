;;; File asdf-defdoc.asd
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; DefDoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with DefDoc.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.

(defpackage :asdf-defdoc-asd (:use :common-lisp :asdf))
(in-package :asdf-defdoc-asd)

(defsystem :asdf-defdoc
    :description "Defdoc integration with ASDF"
    :version "1.0.0"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :components ((:module "asdf" :components
                          (;; The ASDF-DEFDOC package.
                           (:file "package")

                           ;; An NST-testable ASDF class
                           (:file "system"  :depends-on ("package"))))))
