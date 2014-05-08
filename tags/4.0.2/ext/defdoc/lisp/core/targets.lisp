;;; File targets.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; DefDoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with DefDoc.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.

(in-package :defdoc-core)

(def-target-type function (:symbol-definition-checker
                           (lambda (s) (and (fboundp s)
                                            (not (macro-function s)))))
  (:docstring-installer (name spec)
    (setf (documentation name 'function)
          (with-output-to-string (stream)
            (format-docspec stream *docstring-style* spec 'function)))))

(def-target-type compiler-macro (:symbol-definition-checker compiler-macro-function)
  (:docstring-installer (name spec)
    (setf (documentation name 'compiler-macro)
          (with-output-to-string (stream)
            (format-docspec stream *docstring-style* spec 'compiler-macro)))))

(def-target-type macro (:symbol-definition-checker macro-function)
  (:docstring-installer (name spec)
    (let ((docstring (with-output-to-string (stream)
                       (format-docspec stream *docstring-style* spec 'macro))))
      (setf (documentation name 'function) docstring
            (documentation (macro-function name) 'function) docstring))))

(def-target-type setf (:symbol-definition-nocheck t)
  (:docstring-installer (name spec)
    (setf (documentation name 'setf)
          (with-output-to-string (stream)
            (format-docspec stream *docstring-style* spec 'setf)))))

(def-target-type type (:symbol-definition-checker
                       (lambda (e)
                         (let ((class-def (find-class e nil)))
                           (and class-def
                                (not (typep class-def 'structure-class))))))
  (:docstring-installer (name spec)
    (setf (documentation name 'type)
          (with-output-to-string (stream)
            (format-docspec stream *docstring-style* spec 'type)))))

(def-target-type structure (:symbol-definition-checker
                            (lambda (e)
                              (let ((class-def (find-class e nil)))
                                (and class-def
                                     (typep class-def 'structure-class)))))
  (:docstring-installer (name spec)
    (setf (documentation name 'structure)
          (with-output-to-string (stream)
            (format-docspec stream *docstring-style* spec 'structure)))))

(def-target-type package (:symbol-definition-checker find-package)
  (:docstring-installer (name spec)
    (setf (documentation (find-package name) t)
          (with-output-to-string (stream)
            (format-docspec stream *docstring-style* spec 'package)))))

(def-target-type method-combination
    (:lower-case "method combination" :symbol-definition-nocheck t)
  (:docstring-installer (name spec)
    (setf (documentation name 'method-combination)
          (with-output-to-string (stream)
            (format-docspec stream *docstring-style* spec
                            'method-combination)))))

(def-target-type variable (:symbol-definition-checker boundp)
  (:docstring-installer (name spec)
    (setf (documentation name 'variable)
          (with-output-to-string (stream)
            (format-docspec stream *docstring-style* spec 'variable)))))

(def-target-type method (:symbol-definition-nocheck t))

(def-target-type symbol (:symbol-definition-nocheck t))
(def-target-type param (:symbol-definition-nocheck t))
(def-target-type keyword (:symbol-definition-nocheck t))

