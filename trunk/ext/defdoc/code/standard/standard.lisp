;;; File standard-doc.lisp
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
        (o (gensym))
        (o-used nil))
    (loop for (var bound accessor slot) in specs do
      (when (or var bound)
        (setf results `((let (,@(when var `((,var nil)))
                              ,@(when bound `((,bound nil))))
                          (when (slot-boundp ,o ',slot)
                            (setf ,@(when var `(,var (,accessor ,o)))
                                  ,@(when bound `(,bound t))))
                          ,@results))
              o-used t)))
    (cond
      ((and specs o-used) `(let ((,o ,inst)) ,@results))
      (specs `(progn ,@results))
      ((null results) nil)
      ((eql 1 (length results)) (car results))
      (t `(progn ,@results)))))

(defmacro with-unpacked-standard-spec ((self intro intro-supp-p
                                             params params-supp-p
                                             blurb blurb-supp-p
                                             details details-supp-p
                                             callspec)
                                       instance &body forms)
  `(let (,@(when self `((,self (docspec-self ,instance))))
         ,@(when callspec `((,callspec (docspec-callspecs ,instance)))))
     (with-possibly-unbound-slotaccessors
         ((,intro   ,intro-supp-p
                    defdoc-standard-model:docspec-intro
                    defdoc-standard-model::intro)
          (,params  ,params-supp-p
                    defdoc-standard-model:docspec-params
                    defdoc-standard-model::params)
          (,blurb   ,blurb-supp-p
                    defdoc-standard-model:docspec-blurb
                    defdoc-standard-model::blurb)
          (,details ,details-supp-p
                    defdoc-standard-model:docspec-details
                    defdoc-standard-model::details))
         ,instance
       ,@forms)))

(defmethod format-docspec (stream style (spec standard-doc-spec) type
                                  &rest keyargs)
  (apply #'format-standard-docspec-intro-section style type spec stream keyargs)
  (apply #'format-standard-docspec-usage-section style type spec stream keyargs)
  (apply #'format-standard-docspec-details-section
         style type spec stream keyargs)
  )

(defgeneric format-standard-docspec-intro-section
    (style type spec stream &key &allow-other-keys)
  (:method (style type spec stream &key &allow-other-keys)
    (with-unpacked-standard-spec (nil intro intro-supp-p nil nil
                                      blurb blurb-supp-p nil details-supp-p
                                      nil) spec
      (cond
        (intro-supp-p
         (format-docspec-element style type
                                 (get-element-for-docspec-format
                                  style type spec :intro intro)
                                 stream))
        ((and blurb-supp-p (not details-supp-p))
         (format-docspec-element style type
                                 (get-element-for-docspec-format
                                  style type spec :blurb blurb)
                                 stream))))))

(defgeneric format-standard-docspec-usage-section
    (style type spec stream &key &allow-other-keys)
  (:method (style type spec stream &rest keyargs)
    (with-unpacked-standard-spec (nil nil nil nil params-supp-p
                                       nil nil nil nil callspec) spec
      (when callspec
        (apply #'format-standard-docspec-literal-text style
               (with-output-to-string (str-out)
                 (apply #'format-standard-docspec-callspec
                        style type spec str-out keyargs))
               stream keyargs))

      (when params-supp-p
        (apply #'format-standard-docspec-param-list
               style type spec stream keyargs)))))

(defgeneric format-standard-docspec-literal-text (style text stream
                                                        &key &allow-other-keys))

(defgeneric format-standard-docspec-callspec (style type spec stream
                                                    &key &allow-other-keys))

(defgeneric format-standard-docspec-param-list (style type spec stream
                                                      &key &allow-other-keys)
  (:method (style type spec stream &rest keyargs)
    (with-unpacked-standard-spec (nil nil nil params nil nil nil nil nil nil)
        spec
      (apply #'format-standard-docspec-param-list-start
             style type spec stream keyargs)
      (loop for (name subspec) in params do
        (apply #'format-standard-docspec-param-list-item
               style type spec name subspec stream keyargs))
      (apply #'format-standard-docspec-param-list-stop
             style type spec stream keyargs))))
(defgeneric format-standard-docspec-param-list-start
    (style type spec stream &key &allow-other-keys)
  (:method (style type spec stream &key &allow-other-keys)
    (declare (ignore style type spec stream))))
(defgeneric format-standard-docspec-param-list-stop
    (style type spec stream &key &allow-other-keys)
  (:method (style type spec stream &key &allow-other-keys)
    (declare (ignore style type spec stream))))

(defgeneric format-standard-docspec-param-list-item
    (style type spec name subspec stream &key &allow-other-keys)
  (:method (style type spec name subspec stream &rest keyargs)
    (apply #'format-standard-docspec-param-list-item-start
           style type subspec name stream keyargs)
    (format-docspec stream style
                    (get-element-for-docspec-format style type spec
                                                    :params subspec)
                    type)
    (apply #'format-standard-docspec-param-list-item-stop
           style type subspec name stream keyargs)))
(defgeneric format-standard-docspec-param-list-item-start
    (style type spec name stream &key &allow-other-keys)
  (:method (style type spec name stream &key &allow-other-keys)
    (declare (ignore style type spec name stream))))
(defgeneric format-standard-docspec-param-list-item-stop
    (style type spec name stream &key &allow-other-keys)
  (:method (style type spec name stream &key &allow-other-keys)
    (declare (ignore style type spec name stream))))

(defgeneric format-standard-docspec-details-section
    (style type spec stream &key &allow-other-keys)
  (:method (style type spec stream &key &allow-other-keys)
    (with-unpacked-standard-spec (nil nil nil nil nil nil nil
                                      details details-supp-p nil) spec
      (when details-supp-p
        (format-docspec stream style
                        (get-element-for-docspec-format style type spec
                                                        :details details)
                        type)))))

(defgeneric get-element-for-docspec-format (style target-type spec
                                                  element datum)
  (:method (style target-type spec element datum)
     (declare (ignore style target-type spec element))
     datum))

(defmethod print-object ((spec standard-doc-spec) stream)
  (print-unreadable-object (spec stream :type t :identity nil)
    (format stream "~a ~a" (docspec-self spec) (docspec-target-type spec))))
(set-pprint-dispatch 'standard-doc-spec
  (named-function pprint-standard-doc-spec
    (lambda (stream spec)
      (cond
        ((and (boundp '*pprint-short-spec*)
              (symbol-value '*pprint-short-spec*))
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
                       do (format stream "~s ~s" label value)
                          (pprint-exit-if-list-exhausted)
                          (pprint-newline :mandatory stream)))))
           (loop for slot in '(defdoc-core::tags
                               defdoc-core::target-type defdoc-core::self
                               descriptive intro blurb
                               details params callspecs deprecated)
               do (cond
                    ((slot-boundp spec slot)
                     (format stream "~:@_  - ~a ~w"
                       slot (slot-value spec slot)))
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
