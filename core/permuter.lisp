;;; File permuter.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
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

(defclass permuter-frame ()
     ((next-firsts :initarg :next :type (or null cons))
      (prev-firsts :initform nil :type (or null cons))))

(defun whittlable (pf)
  (if (null pf)
      nil
      (with-slots (next-firsts) (car pf)
        (null next-firsts))))

(defclass permuter ()
     ((next-permutation :type (or null cons))
      (perm-stack :type (or null (cons permuter-frame)))
      (has-next :initform t :reader has-next)
      (degenerate)))

(defun get-unassigned (p)
  (with-slots (perm-stack) p
    (with-slots (next-firsts prev-firsts) (car perm-stack)
      (append next-firsts prev-firsts))))

(defmethod initialize-instance :after ((p permuter) &key src &allow-other-keys)
  (with-slots (next-permutation perm-stack has-next degenerate) p
    (setf  next-permutation nil  perm-stack nil  has-next t
           degenerate (null src)))
  (when src (tighten-stack p src)))

(defmethod print-object ((p permuter) stream)
  (print-unreadable-object (p stream :type t :identity nil)
    (fmt-permuter stream p)))

(defgeneric fmt-permuter (stream p)
  (:method (stream (p permuter))
     (with-slots (next-permutation perm-stack has-next) p
       (cond
         (has-next
          (pprint-logical-block (stream '(1 2))
            (format stream "Next permutation ~s;~_ stack " next-permutation)
            (pprint-logical-block (stream perm-stack)
              (loop for perm = (pprint-pop) while perm do
                    (fmt-permuter stream perm)
                    (pprint-exit-if-list-exhausted)
                    (princ ", " stream)
                    (pprint-newline :linear stream)))))
         (t
          (format stream "No more permutations")))))
  (:method (stream (pf permuter-frame))
     (with-slots (next-firsts prev-firsts) pf
       (pprint-logical-block (stream '(1 2))
         (format stream "~s" next-firsts)
         (pprint-newline :linear stream)
         (format stream "~s" prev-firsts)))))

(defun next-permutation (p)
  (with-slots (next-permutation perm-stack has-next degenerate) p
    (if degenerate
        (progn (setf has-next nil)
               next-permutation)
        (let ((result next-permutation))
          (unless has-next
            (error "Asked an empty permutation generator for another\
 permutation"))
          (relax-stack p)
          (let ((whittled (whittle-relaxed-stack p)))
            (if whittled
                (progn
                  (setf has-next nil))
                (tighten-stack p (get-unassigned p))))
          result))))

(defun relax-stack (p)
  (with-slots (next-permutation perm-stack has-next) p
    (unless has-next
      (error
       "Asked an empty permutation generator for another permutation"))
    (let ((reclaimed (pop next-permutation)))
        (with-slots (prev-firsts) (car perm-stack)
          (push reclaimed prev-firsts)))))

(defun whittle-relaxed-stack (p)
  (block main
    (with-slots (next-permutation perm-stack has-next) p
      (block bleh
        (loop do
          (unless (and perm-stack
                       (with-slots (next-firsts) (car perm-stack)
                         (null next-firsts)))
            (return-from bleh))
          (pop perm-stack)
          (when (null perm-stack)
            (return-from main t))
          (relax-stack p)))
      (with-slots (next-firsts) (car perm-stack)
        (let ((new-prefix (pop next-firsts)))
          (push new-prefix next-permutation)))
      nil)))

(defun tighten-stack (p slottable)
  (with-slots (next-permutation perm-stack has-next) p
    (loop for sublist on slottable do
      (unless (null sublist)
        (destructuring-bind (head &rest tail) sublist
          (push head next-permutation)
          (push (make-instance 'permuter-frame :next tail)
                perm-stack))))))

;;;(defmacro while-permutations (x permutations &rest forms)
;;;  `(loop while (has-next ,permutations) do
;;;    (let ((,x (next-permutation ,permutations)))
;;;      ,@forms)))

(defparameter qwerty (make-instance 'permuter :src '(q w e r t y)))
(defparameter qw (make-instance 'permuter :src '(q w)))

