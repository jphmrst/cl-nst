;;; File defdoc-ex.asd
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

(defpackage :defdoc-ex-asd (:use :common-lisp :asdf))
(in-package :defdoc-ex-asd)

(defsystem :defdoc-ex
    :description "Example using structured document specifiers"
    :depends-on (:defdoc)
    :components (;; The DEFDOC package, plus internal packages
                 ;; and documentation generation.
                 (:file "package")

                 ;; Helper functions.
                 (:file "ex"  :depends-on ("package"))

                 ))
