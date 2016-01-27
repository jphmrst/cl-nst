;;; CURRENTLY EXCLUDED

;;; File context.lisp
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

(defclass context-layer () ()
  (:documentation "Superclass of records of test criterion."))

(defmacro with-context-layer (layer-expr &body body)
  `(let ((*nst-context* (cons ,layer-expr *nst-context*)))
     (declare (special *nst-context*))
     ,@body))

(defgeneric show-context-layer (layer)
  (:documentation "Used to determine whether a context-layer should be displayed.  Methods may safely refer and mutate the hashtable stored in the special variable -context-display-state- .")
  (:method (l) (declare (ignore l)) nil))

(defun get-display-context-layers (layers)
  (let ((-context-display-state- (make-hash-table)))
    (declare (special -context-display-state-))
    (loop for layer in layers
          if (show-context-layer layer)
            collect layer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass criterion-context-layer (context-layer)
     ((criterion :initarg :criterion :accessor criterion)
      (criterion-args :initarg :criterion-args :accessor criterion-args)
      (given-stack :initarg :given-stack :accessor given-stack))
  (:documentation "A record of test criterion
 criterion - the criterion symbol itself
 criterion-args - arguments to the criterion
 given-stack - the stack of values assessed by the criterion"))

(set-pprint-dispatch 'criterion-context-layer
  (named-function pprint-criterion-context-layer
    (lambda (s cl)
      (with-accessors ((criterion criterion)
                       (criterion-args criterion-args)
                       (given-stack given-stack)) cl
        (cond
          ((eq (car given-stack) 'list)
           (princ "checki" s)
           (pprint-logical-block (s '(1 2))
             (format s "ng (~s" criterion)
             (pprint-logical-block (s criterion-args)
               (loop for arg = (pprint-pop) while arg do
                     (pprint-newline :fill s)
                     (princ " " s)
                     (format s "~s" arg)))
             (princ ") " s)
             (pprint-newline :linear s)
             (princ "on" s)
             (loop for layer in (cdr given-stack) do
                   (format s " ~a" layer))))

          (t (princ "ch" s)
             (pprint-logical-block (s '(1 2))
               (format s "ecking (~s" criterion)
               (pprint-logical-block (s criterion-args)
                 (loop for arg = (pprint-pop) while arg do
                       (pprint-newline :fill s)
                       (princ " " s)
                       (format s " ~s" arg)))
               (princ ") " s)
               (pprint-newline :fill s)
               (princ-filled-text "on the result of evaluating " s)
               (pprint-newline :fill s)
               (format s "~a" given-stack))))))))

(defmethod show-context-layer ((layer criterion-context-layer))
  (declare (special -context-display-state-))
  (cond
   ((or (> *nst-verbosity* 2)
        (and (> *nst-verbosity* 1) (eq *nst-report-driver* :details)))
    t)
   ((gethash 'criterion -context-display-state-)
    nil)
   (t
    (setf (gethash 'criterion -context-display-state-) t)
    t)))

;;;(defmacro within-context ((name args values) &body forms)
;;;  `(let ((*nst-context* (cons (make-context-layer
;;;                               :criterion ',name
;;;                               :criterion-args ',args
;;;                               :given-stack ,(cond
;;;                                              (*nst-context-evaluable*
;;;                                               values)
;;;                                              (t `',values)))
;;;                              *nst-context*)))
;;;     (declare (special *nst-context*))
;;;     ,@forms))

(defmacro with-criterion-context-layer ((&rest layer-args) &body body)
  `(with-context-layer (make-instance 'criterion-context-layer ,@layer-args)
     ,@body))

(defun get-local-criterion-context (context-layer-list)
  (cond
    ((null context-layer-list) nil)
    (t (let ((top (car context-layer-list)))
         (cond
           ((typep top 'criterion-context-layer)
            (criterion top))
           (t (get-local-criterion-context (cdr context-layer-list))))))))

