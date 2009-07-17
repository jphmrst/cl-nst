;;; File pick.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
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

;;; Support for smart picking of artifacts to be run without needing
;;; explicit package or (for tests) group names.

;;
;; Reference package for name lookup.
;;
(defpackage :nst-artifact-lookup-package
    (:documentation "Auxiliary package for storing NST names for lookup."))
(defgeneric canonicalize-lookup-name (o)
  (:method ((s string)) (intern s :nst-artifact-lookup-package))
  (:method ((s symbol)) (intern (symbol-name s) :nst-artifact-lookup-package)))

;;
;; Single point of artifact lookup.
;;
(defparameter +artifact-lookup+ (make-hash-table :test 'eq)
  "Hash table for looking up name usage.  Symbols in
:nst-artifact-lookup-package are mapped to a set of denoted items stored in a
hash-table mapping items to t.")

(defun note-artifact (name artifact)
  "Standardize the name (a string), and file the artifact."
  (let* ((name-symbol (canonicalize-lookup-name name))
         (name-set (gethash name-symbol +artifact-lookup+)))
    (unless name-set (setf name-set (make-hash-table :test 'eq)
                           (gethash name-symbol +artifact-lookup+) name-set))
    (setf (gethash artifact name-set) t)))

(defun note-executable (name artifact)
  (when (group-record-p artifact)
    (let ((package (symbol-package name)))
      (note-artifact (package-name package) package)))
  (note-artifact (symbol-name name) artifact))

;;
;; Record explicit run choices, for when we have many.
;;
(defparameter +last-named-artifact-use+ (make-hash-table :test 'eq))

(defun note-artifact-choice (name item)
  (setf (gethash (canonicalize-lookup-name name) +last-named-artifact-use+)
        item))

;;
;; Determine what artifact is intended.
;;
(defun lookup-artifact (name)
  "Input is a symbol, output is a list."
  (let* (;; Our internal translation for looking up names.
         (canonical (canonicalize-lookup-name name))
         ;; List of the artifacts corresponding to that name.
         (content (loop for key being the hash-keys
                        of (gethash canonical +artifact-lookup+) collect key)))
    (cond
      ;; If there's more than one thing by that name, see if we've
      ;; recently used one.
      ((> (length content) 1)
       (let ((last (gethash canonical +last-named-artifact-use+)))
         (cond
           (last (list last))
           (t content))))

      ;; Otherwise, return the original (singleton or empty) list.
      (t content))))
