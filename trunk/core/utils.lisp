;;; File utils.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
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
(in-package :sift.nst)

;;;
;;; Generic functions whose methods are defined by the various macros.
;;;
(defmacro add-class-name-static-method (fn)
  `(progn
     (defmethod ,fn ((g symbol)) (,fn (make-instance g)))
     ;; (defmethod ,fn ((g standard-class)) (,fn (make-instance g)))
     ))

(defmacro add-class-name-instantiator-method (fn)
  `(defmethod ,fn ((g symbol)) (,fn (make-instance g))))


;;;
;;; Helper functions
;;;

;; Versions of car and cdr for when we expect to have either a list or
;; a symbol.

(defun symbol-or-car (name-or-name-and-args)
  "Return the first element given a list, or return a symbol."
  (cond ((symbolp name-or-name-and-args) name-or-name-and-args)
        ((consp name-or-name-and-args) (car name-or-name-and-args))
        (t (cerror "Return nil" "Unable to parse ~S to find the name in it."
                   name-or-name-and-args)
           nil)))

(defun cdr-or-nil (name-or-name-and-args)
  "Return the cdr given a list, or return nil if given a symbol."
  (cond ((symbolp name-or-name-and-args) nil)
        ((listp name-or-name-and-args) (cdr name-or-name-and-args))
        (t (cerror "Return nil" "Unable to parse ~S to find the name in it."
                   name-or-name-and-args)
           nil)))

;; Tests on numbers.

(defmacro log10 (v) `(/ (log ,v) (log 10)))

(defun sig-place (n value)
  "Returns the n-th significant place of value"
  (let* ((xlog (if (zerop value) 0 (log10 (abs value))))
         (xlog-up (floor xlog)))
    (expt 10 (- xlog-up (- n 1)))))

(defun eql-for-sigdigits (digits n1 n2)
  "Test whether two numbers are eql to the given number of significant digits."
  (and (numberp n1) (numberp n2)
       (let ((rounder (sig-place digits n1)))
         (eql (round n1 rounder) (round n2 rounder)))))

;; Define this macro on platforms which do not provide it.

#-allegro
(defmacro named-function (name lambda-expression)
  (declare (ignore name))
  `(function ,lambda-expression))
