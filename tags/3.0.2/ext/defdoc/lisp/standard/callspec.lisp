;;; File callspec.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; DefDoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with DefDoc.  If not, see
;;; <http://www.gnu.org/licenses/>.

(in-package :defdoc-standard-model)

;;; Decoding the :callspec forms.

(defclass standard-callspec ()
     ((mandatory :initarg :mandatory :accessor standard-callspec-mandatory)
      (optional :initarg :optional :accessor standard-callspec-optional)
      (optional-supp :initarg :optional-supp
                     :accessor standard-callspec-optional-supp)
      (key :initarg :key :accessor standard-callspec-key)
      (key-supp :initarg :key-supp :accessor standard-callspec-key-supp)
      (body :initarg :body :accessor standard-callspec-body)
      (body-supp :initarg :body-supp :accessor standard-callspec-body-supp)))

(defclass macrolist-callspec (standard-callspec) ())

(defgeneric get-compiled-callspec (package target-type callspec
                                           &optional actual-class)
  (:method (package target-type callspec
                    &optional (actual-class 'standard-callspec))
     (let ((mandatory nil)
           (keyword nil)
           (optional nil)
           (body nil)
           (keyword-supp nil)
           (optional-supp nil)
           (body-supp nil)
           (phase 0)
           (last nil))
       (loop for form in callspec do
         (flet ((unphased ()
                  (error "Found ~s in callspec~@[ after ~s~]" form last)))
           (macrolet ((else-unphased (cnd &body body)
                        `(cond
                          (,cnd ,@body)
                          (t (unphased)))))
             (cond
              ((eq form '&key)
               (else-unphased (eql phase 0) (setf phase 1 last form
                                                  keyword-supp t)))

              ((eq form '&optional)
               (else-unphased (eql phase 0) (setf phase '&optional last form
                                                  optional-supp t)))

              ((or (eq form '&rest) (eq form '&body))
               (else-unphased (and (numberp phase) (< phase 2))
                              (setf phase 2 last form body-supp t)))

              ((eq phase '&optional)  (push form optional))
              ((eql phase 0)
               (push (get-compiled-callspec-simple-item package
                                                        target-type form)
                     mandatory))
              ((eql phase 1)
               (push (get-compiled-callspec-keyarg-item package
                                                        target-type form)
                     keyword))
              ((eql phase 2)
               (push (get-compiled-callspec-simple-item package
                                                        target-type form)
                     body))))))
       (make-instance actual-class
         :mandatory (nreverse mandatory)
         :optional (nreverse optional) :optional-supp optional-supp
         :key (nreverse keyword) :key-supp keyword-supp
         :body (nreverse body)   :body-supp body-supp))))

(defclass callspec-items-holder ()
     ((items :initarg :items :reader get-callspec-holder-items)))
(defclass callspec-bag-of (callspec-items-holder) ())
(defclass callspec-one-of (callspec-items-holder) ())
(defclass callspec-sequence-of ()
     ((repeated :initarg :repeated :reader get-callspec-sequence-of-repeated)))
(defclass callspec-optional ()
     ((option :initarg :option :reader get-callspec-optional-option)))
(defclass callspec-keyheaded ()
     ((key   :initarg :key   :reader get-callspec-keyheaded-key)
      (forms :initarg :forms :reader get-callspec-keyheaded-forms)))

(defun get-compiled-callspec-simple-item (package target-type item)
  (cond
    ((symbolp item) item)
    ((listp item)
     (case (car item)
       ((:seq)
        (make-instance 'callspec-sequence-of
          :repeated (loop for sub in (cdr item)
                        collect (get-compiled-callspec-simple-item
                                 package target-type sub))))
       ((:bag)
        (make-instance 'callspec-bag-of
          :items (loop for sub in (cdr item)
                     collect (get-compiled-callspec-simple-item
                              package target-type sub))))
       ((:alt)
        (make-instance 'callspec-one-of
          :items (loop for sub in (cdr item)
                     collect (get-compiled-callspec-simple-item
                              package target-type sub))))
       ((:opt)
        (make-instance 'callspec-optional
          :option (loop for sub in (cdr item)
                      collect (get-compiled-callspec-simple-item
                               package target-type sub))))
       ((:key-head)
        (make-instance 'callspec-keyheaded
          :key (cadr item)
          :forms (loop for sub in (cddr item)
                     collect (get-compiled-callspec-simple-item
                              package target-type sub))))
       (otherwise (get-compiled-callspec package target-type item
                                         'macrolist-callspec))))
    (t (error "Unrecognized lambda list element ~s" item))))

(defclass callspec-keyarg ()
     ((key :initarg :key :reader get-callspec-keyarg-key)
      (arg :initarg :arg :reader get-callspec-keyarg-arg)))

(defun get-compiled-callspec-keyarg-item (package target-type item)
  (destructuring-bind (keyword param-desig) item
    (make-instance 'callspec-keyarg
      :key keyword
      :arg (get-compiled-callspec-simple-item package
                                              target-type param-desig))))

