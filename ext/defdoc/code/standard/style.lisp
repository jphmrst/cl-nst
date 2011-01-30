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

(defclass docspec-par-latex-style () ())

(defmethod format-output-contents-sep ((style docspec-par-latex-style)
                                       stream output spec1 spec2
                                       &key &allow-other-keys)
  (declare (ignore output spec1 spec2))
  (format stream "\\par "))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defclass docspec-fancy-header-latex-style (docspec-par-latex-style)
  ())

(defgeneric default-format-fancy-header-target-type (style target-type
                                                           spec name stream)
  (:method (style target-type spec name stream)
    (declare (ignore spec name style))
    (princ (capitalized-target-name (get-target-type target-type)) stream)))

(defgeneric format-fancy-header-target-type (style target-type spec name stream)
  (:method ((style docspec-fancy-header-latex-style)
            target-type spec name stream)
    (default-format-fancy-header-target-type style target-type
      spec name stream))
  (:method ((style docspec-fancy-header-latex-style)
            (target-type (eql 'function)) spec name stream)
    (declare (ignore spec))
    (cond
     ((and (fboundp name) (typep (symbol-function name) 'generic-function))
      (princ "Generic function" stream))
     (t (call-next-method)))))

(defmethod format-docspec
    :before (stream (style docspec-fancy-header-latex-style)
                    (spec standard-doc-spec) target-type &key &allow-other-keys)
  (let ((self (docspec-self spec)))
    (multiple-value-bind (home-package exported-p)
        (locate-package-home style target-type spec self)
      (princ "\\vspace{1ex}\\noindent\\begin{tabular}{@{}p{\\textwidth}@{}}\\bfseries\\itshape " stream)
      (format-fancy-header-target-type style target-type spec self stream)
      (format stream " ~a\\hspace*{\\fill}~a :~a\\\\\\hline\\end{tabular}\\\\"
              (symbol-name self)
              (if exported-p "Package" "Internal to")
              (package-name home-package)))))
