;;; File package.lisp
;;;
;;; This file is part of the Defdoc unit/regression testing system.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
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
;;; License along with Defdoc.  If not, see
;;; <http://www.gnu.org/licenses/>.

(in-package :common-lisp-user)

(defpackage :sift.asdf-defdoc
    (:documentation "Documentation unity for Common Lisp")
    (:nicknames :asdf-defdoc)
    (:use :common-lisp :asdf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(asdf::defdoc-asdf) (find-package :sift.asdf-defdoc))
  (export '(asdf::defdoc-asdf) (find-package :asdf)))



