;;; File defcontract-doc.asd
;;;
;;; This file is part of the DefContract API contract enforcement system.
;;;
;;; Copyright (c) 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefContract is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; DefContract is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with DefContract.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.

(defpackage :defcontract-doc-asd (:use :common-lisp :asdf))
(in-package :defcontract-doc-asd)

(asdf:oos 'asdf:load-op :asdf-defContract)

(defsystem :defcontract-doc
  :description "Documentation builder for defcontract"
  :class defContract-asdf
  :depends-on ( :defContract :asdf-defContract :defcontract )
  :documents-system :defcontract
  :components ((:module "doc" :components
                        (;; The NST package, plus internal packages
                         ;; and documentation generation.
                         (:file "package")

                         ;; Helper functions.
                         (:file "doc"  :depends-on ("package")))))
  :documentation-package :defcontract-doc
  :build-output ((#:defcontract-manual :rel-directory "doc/"
                   :filename-root "html"
                   :style #:html-style)
                 (#:defcontract-manual :rel-directory "doc/"
                   :style #:manual-style)
                 (#:defcontract-manual :rel-directory "doc/"
                   :style #:plaintext-style)))
