;;; File context.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2010 Smart Information Flow Technologies.
;;; Written by John Maraist.
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
  #'(lambda (s cl)
      (with-accessors ((criterion criterion)
                       (criterion-args criterion-args)
                       (given-stack given-stack)) cl
        (cond
         ((eq (car given-stack) 'list)
          (format s
              #-(or sbcl scl) "checki~@<ng (~s~@<~{~:_ ~s~}~:>) ~_on~{ ~a~}~:>"
              #+(or sbcl scl) "checking (~s~{~:_ ~s~}) on~{ ~a~}"
              criterion criterion-args (cdr given-stack)))

         (t
          (format s
              #-(or sbcl scl) "ch~@<ecking (~s~@<~{~:_ ~s~}~:>) ~
                        ~:_on ~:_the ~:_result ~:_of ~:_evaluating ~:_~a~:>"
              #+(or sbcl scl) "checking (~s~{~:_ ~s~}) on the result of evaluating ~a"
              criterion criterion-args given-stack))))))

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

