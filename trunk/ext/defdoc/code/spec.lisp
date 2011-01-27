;;; File spec.lisp
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
(in-package :defdoc)

(defvar *spec-class* 'standard-doc-spec)
(defgeneric get-spec-class (package name forms)
  (:method (package name forms)
    (declare (ignore package name forms))
    *spec-class*))

(defclass doc-spec (labeled)
  ((self :initarg :self :reader docspec-self)
   (target-type :initarg :target-type :reader docspec-target-type)
   (tags :initarg :tags :accessor docspec-tags)))

(defmethod format-doc (stream style (spec doc-spec))
  (format-docspec stream style spec (docspec-target-type spec)))

(defclass standard-doc-spec (doc-spec)
     ((descriptive :initarg :descriptive :accessor docspec-descriptive)
      (intro  :initarg :intro  :accessor docspec-intro)
      (blurb  :initarg :blurb  :accessor docspec-blurb)
      (details   :initarg :details   :accessor docspec-details)
      (params :initarg :params :accessor docspec-params)
      (callspecs :initarg :callspecs :initform nil :accessor docspec-callspecs)
      (deprecated :initarg :deprecated :accessor docspec-deprecated)))

(defmacro with-possibly-unbound-slotaccessors (specs inst &body body)
  (let ((results body)
        (o (gensym)))
    (loop for (var bound accessor slot) in specs do
      (setf results `((let ((,var nil) (,bound nil))
                        (when (slot-boundp ,o ',slot)
                          (setf ,var (,accessor ,o) ,bound t))
                        ,@results))))
    (cond
      (specs `(let ((,o ,inst)) ,@results))
      ((null results) nil)
      ((eql 1 (length results)) (car results))
      (t `(progn ,@results)))))

(defmacro with-unpacked-standard-spec ((self intro intro-supp-p
                                             params params-supp-p
                                             blurb blurb-supp-p
                                             details details-supp-p
                                             callspec)
                                       instance &body forms)
  `(let ((,self (docspec-self ,instance))
         (,callspec (docspec-callspecs ,instance)))
     (with-possibly-unbound-slotaccessors
         ((,intro  ,intro-supp-p  docspec-intro  intro)
          (,params ,params-supp-p docspec-params params)
          (,blurb  ,blurb-supp-p  docspec-blurb  blurb)
          (,details   ,details-supp-p   docspec-details   details))
         ,instance
       ,@forms)))

(set-pprint-dispatch 'standard-doc-spec
  (named-function pprint-standard-doc-spec
    (lambda (stream spec)
      (cond
       ((and (boundp '*pprint-short-spec*) (symbol-value '*pprint-short-spec*))
        (format stream "[[ ~a ~a ]]"
                (docspec-target-type spec) (docspec-self spec)))
       (t
        (pprint-logical-block (stream '(1))
          (format stream "[ standard-doc-spec")
          (let ((props (label-values spec)))
            (when (< 0 (hash-table-count props))
              (format stream "~:@_  - properties: ")
              (pprint-logical-block
                  (stream (loop for label being the hash-keys of props
                            collect label))
                (loop for label = (pprint-pop)
                    for value = (gethash label props)
                    do
                      (format stream "~s ~s" label value)
                      (pprint-exit-if-list-exhausted)
                      (pprint-newline :mandatory stream)))))
          (loop for slot in '(tags
                              target-type self descriptive intro blurb details
                              params callspecs deprecated)
              do
                (cond
                 ((slot-boundp spec slot)
                  (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
                 (t (format stream "~:@_  - no ~a" slot))))
          (format stream " ]")))))))

(defun compile-spec (name target-type spec-args forms)
  (declare (ignore spec-args))
  (get-compiled-spec (symbol-package name) target-type name forms))

(defgeneric get-compiled-spec (package target-type name forms)
  (:method (package target-type name forms)
     (let* ((use-class (get-spec-class package name forms))
            (result (make-instance use-class
                      :self name :target-type target-type)))
       (loop for form in forms do
         (let ((hd (car form))
               (tl (cdr form)))
           (process-standard-specdef-form package target-type
                                          hd name tl result)))
       result)))

(defgeneric process-standard-specdef-form (package target-type form-head
                                           target-name form-args spec)
  (:method (package target-type form-head target-name form-args spec)
     (declare (ignore target-type))
     (macrolet ((setting-accessor (acc)
                  `(setf (,acc spec)
                         (compile-element package spec form-args))))
       (case form-head
         ((:deprecated)  (setf (docspec-deprecated spec)  (car form-args)))
         ((:descriptive) (setf (docspec-descriptive spec) (car form-args)))
         ((:tags) (setf (docspec-tags spec) form-args))
         ((:params)
          (setf (docspec-params spec)
                (loop for (id forms) in form-args
                  collect (list id (compile-element package spec forms)))))
         ((:intro)       (setting-accessor docspec-intro))
         ((:blurb)       (setting-accessor docspec-blurb))
         ((:details)        (setting-accessor docspec-details))
         ((:callspec) (setf (docspec-callspecs spec)
                            (mapcar (named-function standard-specdef-mapper
                                      (lambda (x)
                                        (get-compiled-callspec package spec x)))
                                    form-args)))
         ((:properties) (loop for (name value) in form-args do
                          (setf (label-value spec name) value)))
         (otherwise
          (error "Unrecognized form (~s~{ ~s~}) in docspec body for ~s"
                 form-head form-args target-name))))))
