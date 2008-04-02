;;; File tests.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
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
(in-package :sift.nst)

(defparameter o-id-actual nil)

(defmacro outer (o-id &rest forms)
  (let ((o-id-actual o-id))
    (declare (dynamic-extent o-id-actual) (ignorable o-id-actual))
    (let ((actual-inner (loop for form in forms
			      collect (macroexpand form))))
      `'(,@actual-inner))))

(defmacro inner (i-id)
  (declare (special o-id-actual))
  `'(,o-id-actual ,i-id))
