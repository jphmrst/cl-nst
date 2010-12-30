;;; File defdoc.asd
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; Defdoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Defdoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Defdoc.  If not, please see
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
