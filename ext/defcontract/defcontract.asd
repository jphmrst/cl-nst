;;; File defcontract.asd
;;;
;;; This file is part of the DefContract API checker system.
;;;
;;; Copyright (c) 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefContract is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; Defcontract is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Defcontract.  If not, see
;;; <http://www.gnu.org/licenses/>.

(defpackage :defcontract-asd (:use :common-lisp :asdf))
(in-package :defcontract-asd)

(defsystem :defcontract
    :description "API guarantee enforcement"
                                        ; In the svn repository, the
                                        ; version number is what we're
                                        ; working on now.
                                        ;
                                        ; This version number and the
                                        ; number in VERSIONS.txt must
                                        ; be kept manually
                                        ; synchronized.
                                        ;
                                        ; The patch number (third
                                        ; number) should be
                                        ; incremented immediately
                                        ; after a version is shipped.
    :version "0.0.1"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 3.latest"
    ;; :in-order-to ((test-op (test-op :defcontract-test)))
    :depends-on ( :closer-mop )
    :components
    ((:module "lisp" :components
              (;; The DEFCONTRACT package, plus internal packages and
               ;; documentation generation.
               (:file "package")

               (:file "globals" :depends-on ("package"))

               (:file "contract" :depends-on ("globals"))

               (:file "enforce" :depends-on ("globals"))))))
