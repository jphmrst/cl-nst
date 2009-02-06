;;; File nst.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
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

(defpackage :nst-asd (:use :common-lisp :asdf))
(in-package :nst-asd)

(defclass nst-tester (system) ())

(defsystem :nst
    :serial t
    :version "0.2.3"
    ;; :depends-on (:jm-defs)
    :in-order-to ((test-op (test-op :test-nst)))
    :components ((:file "package")
                 (:file "permuter")
                 (:file "numbers")
                 (:file "globals")
                 (:file "classes")
                 (:file "runners")
                 (:file "fixtures")
                 (:file "testforms")
                 (:file "status")
                 (:file "defcheck")
                 (:file "criteria")
                 (:file "interactive")
                 (:file "format")))

(defclass nst-file (cl-source-file) ())
(defmethod perform ((o compile-op) (c nst-file)) nil)
(defmethod operation-done-p ((op compile-op) (file nst-file)) nil)
(defmethod input-files ((op load-op) (file nst-file))
  (list (component-pathname file)))
(defmethod output-files ((o compile-op) (c nst-file)) nil)

(defsystem :test-nst
    :class nst-tester
    :depends-on (:nst)
    :in-order-to ((test-op (load-op :test-nst)))
    :components ((:nst-file "nst-nst")
                 (:nst-file "nst-interact")
                 (:nst-file "nst-criteria")
                 (:nst-file "nst-fails")))

(defmethod perform ((op test-op)
                    (system (eql (find-system :test-nst))))
  (eval (list (intern (symbol-name '#:run-nst-commands)
                      (find-package :nst))
              :run-package
              (quote (intern (symbol-name 'nst-test)
                             (find-package 'cl-user))))))

(defmethod operation-done-p ((o test-op) (c nst-tester))
  "We need to make sure that operation-done-p doesn't return its
normal value, or a test-op will be run only once."
  (values nil))
