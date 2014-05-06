;;; File package.lisp
;;;
;;; This file is part of the Defdoc unit/regression testing system.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
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

(in-package :common-lisp-user)

(defpackage :sift.asdf-defdoc
    (:documentation "Documentation unity for Common Lisp")
    (:nicknames :asdf-defdoc)
    (:use :common-lisp :asdf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(asdf::defdoc-asdf) (find-package :sift.asdf-defdoc))
  (export '(asdf::defdoc-asdf) (find-package :asdf)))



