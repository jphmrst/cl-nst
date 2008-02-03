;;; File numbers.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst)

#+allegro
(defmacro extract-lambda-list (&rest args)
  `(mop:extract-lambda-list ,@args))
#-(or sbcl allegro)
(eval-when (:compile-toplevel :load-toplevel)
  (error "Don't have method for calling extract-lambda-list from the MOP for this list.  See nst/numbers.lisp."))

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
