;;; File test.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2014 Smart Information Flow Technologies.
;;; Copyright (c) 2015-2016 John Maraist
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
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

(in-package :nst-manual)

;;;(eval-when (:compile-toplevel :load-toplevel)
;;;  (format t "either/before~%")
;;;  (eval-when (:load-toplevel)
;;;    (let ((x 3))
;;;      (defmethod ntest ((c (eql 4))) x))
;;;    (let ((x 5))
;;;      (defmethod ntest ((c (eql 5))) x))
;;;    (format t "load~%"))
;;;  (eval-when (:compile-toplevel)
;;;    (format t "compile~%"))
;;;  (format t "either/after~%"))

(nst::def-group group1t ()
  (nst::def-check check1t (:pass) ()) (nst::def-check check2t (:pass) ()))
