;;; File numbers.lisp
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


;;; Functions supporting tests on numbers.

(defmacro log10 (v) `(/ (log ,v) (log 10)))

(defun sig-place (n value)
  "Returns the n-th significant place of value"
  (let* ((xlog (if (zerop value) 0 (log10 (abs value))))
	 (xlog-up (floor xlog)))
    (expt 10 (- xlog-up (- n 1)))))

(defun eql-for-sigdigits (digits n1 n2)
  (and (numberp n1)
       (numberp n2)
       (let ((rounder (sig-place digits n1)))
	 (eql (round n1 rounder) (round n2 rounder)))))

(defun lambda-list-names (lambda-list)
  (let ((generic-list (extract-lambda-list lambda-list))
	(result))
    (labels ((descend (list)
	        (unless (null list)
		  (let ((item (car list)))
		    (cond 
		     ((listp item)
		      (descend item))
		     ((symbolp item)
		      (unless (member item
				      #+allegro '(&allow-other-keys &aux
						  &body &environment &key
						  &optional &rest &whole)
				      #-allegro lambda-list-keywords)
			(push item result))))
		    (descend (cdr list))))))
      (descend generic-list)
      (nreverse result))))
