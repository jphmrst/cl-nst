;;; File style.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2011 Smart Information Flow Technologies.
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

;;; -----------------------------------------------------------------

(defclass symbol-homing-style ()
  ((symbol-homes :initarg :symbol-homes :accessor symbol-homes)
   (use-internal-names :initform nil :initarg :use-internal-names
                       :accessor use-internal-names)))

(defgeneric candidate-home-packages (style target-type spec))

(defmethod locate-package-home ((style symbol-homing-style)
                                target-type spec symbol)
  (let ((candidates (loop for cand-name
                      in (candidate-home-packages style target-type spec)
                      for cand-package = (find-package cand-name)
                      if cand-package collect cand-package)))
    (loop for package in candidates do
      (multiple-value-bind (echo status)
          (find-symbol (symbol-name symbol) package)
        (when (and echo (eq echo symbol) (eq status :external))
          (return-from locate-package-home (values package t)))))
    (when (use-internal-names style)
      (loop for package in candidates do
        (multiple-value-bind (echo status)
            (find-symbol (symbol-name symbol) package)
          (declare (ignore status))
          (when (and echo (eq echo symbol))
            (return-from locate-package-home (values package nil)))))))
  (call-next-method))

;;; -----------------------------------------------------------------

(defclass itemized-list-style () ())

(defgeneric format-itemized-list-start (style stream))
(defgeneric format-itemized-list-end (style stream))
(defgeneric format-itemized-list-item-start (style stream))
(defgeneric format-itemized-list-item-end (style stream))

(defmethod format-doc-content-item (stream (style itemized-list-style) spec
                                           &key &allow-other-keys)
  (declare (ignore spec))
  (format-itemized-list-item-start style stream)
  (call-next-method)
  (format-itemized-list-item-end style stream))

(defmethod format-output-leader-material ((style itemized-list-style) stream
                                          output &key &allow-other-keys)
  (declare (ignore output))
  (format t "XXXXXXXXXX~%")
  (format-itemized-list-start style stream))
(defmethod format-output-trailer-material ((style itemized-list-style) stream
                                           output &key &allow-other-keys)
  (declare (ignore output))
  (format-itemized-list-end style stream))

(defmacro def-standard-style-class (name superclasses fields
                                         (&rest keyvals &key &allow-other-keys)
                                         &body class-forms)
  `(progn
     (def-style-class ,name ,superclasses ,fields ,keyvals ,@class-forms)
     ))