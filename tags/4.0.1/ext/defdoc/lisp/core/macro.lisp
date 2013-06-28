;;; File macro.lisp
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

(defvar *docspec-compiler-name* 'defdoc-standard-model:compile-spec)

(defmacro def-documentation (name-or-spec &body body)
  "Doc doc doc"
  (multiple-value-bind (name target-type spec-args)
      (decode-defdoc-spec name-or-spec)
    (let ((spec (gensym "spec")))
      `(let ((,spec (funcall (symbol-function *docspec-compiler-name*)
                             ',name ',target-type ',spec-args ',body)))
         (setf (get-doc-spec ',name ',target-type) ,spec)
         (funcall (docstring-installer (get-target-type ',target-type))
                  ',name ,spec)
         ;; (format t "SPEC for ~s:~%~s~%" ',name ,spec)
         ))))

(defun decode-defdoc-spec (name-or-spec)
  (cond
   ((symbolp name-or-spec)
    (values name-or-spec (guess-spec-type name-or-spec) nil))
   ((listp name-or-spec)
    (destructuring-bind (spec-type name &rest spec-args) name-or-spec
      (values name spec-type spec-args)))))

;;; -----------------------------------------------------------------

(defmacro ensure-api-documentation (package &key error warning internal defdoc)
  (let* ((sym (gensym)))
    `(eval-when (:load-toplevel :execute)
       (;; First pick the CL macro for iterating over a package's
        ;; symbols: all internal or only external?
        ,(if internal 'do-symbols 'do-external-symbols)

        ;; Next the iteration variable and the name of the package
        ;; over which we iterate.
        (,sym ,package)

         ;;; In the body of the do-symbols or do-external-symbols
         ;;; iteration --- check the symbol.
         (warn-if-undocumented ,sym :defdoc-only ,defdoc
                               :error ,error :warning ,warning)))))
