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
(in-package :defdoc-core)

(defvar *spec-class* 'defdoc-standard-model:standard-doc-spec)
(defgeneric get-spec-class (package name forms)
  (:method (package name forms)
    (declare (ignore package name forms))
    *spec-class*))

(defclass doc-spec (labeled)
  ((self :initarg :self :reader docspec-self)
   (target-type :initarg :target-type :reader docspec-target-type)
   (tags :initarg :tags :accessor docspec-tags)))

(defmethod format-doc (stream style (spec doc-spec) &rest keyargs)
  (apply #'format-docspec
         stream style spec (docspec-target-type spec) keyargs))
