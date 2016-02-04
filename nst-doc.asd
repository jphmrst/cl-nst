;;; File nst-doc.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2010-2014 Smart Information Flow Technologies.
;;; Copyright (c) 2015-2016 John Maraist
;;; Written by John Maraist and other contributors.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.

(defpackage :nst-doc-asd (:use :common-lisp :asdf))
(in-package :nst-doc-asd)
(asdf:oos 'asdf:load-op :asdf-defdoc)

(defsystem :nst-doc
    :class defdoc-asdf
    :description "Documentation builder for NST"
    :depends-on (:nst :asdf-defdoc :org-sampler)
    :documents-system :nst
    :components ((:module "doc" :components
                          ((:file "package")
                           (:file "doc"  :depends-on ("package"))
                           (:file "manual"   :depends-on ("doc"))
                           (:file "quickref" :depends-on ("doc")))))

    :documentation-package :nst-doc
    :build-output (#-sbcl
                   (#:the-manual :rel-directory "doc/"
                                 :filename-root "html"
                                 :style #:html-style
                                 :index t :table-of-contents t)
                   (#:the-manual :rel-directory "doc/"
                                 :filename-root "nst-manual"
                                 :style #:manual-primary-style
                                 :index t :table-of-contents t)
                   (#:the-manual :rel-directory "doc/"
                                 :filename-root "nst-manual"
                                 :style #:plaintext-style
                                 :index t :table-of-contents t)))

(defmethod asdf:perform ((op load-op)
                         (system (eql (asdf:find-system :nst-doc))))
  (call-next-method)
  (funcall (symbol-function (intern (symbol-name '#:build-quickref) :nst-doc))))
