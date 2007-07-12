;;; File nst.asd
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman
;;;
;;; NST is Copyright (c) 2006 Smart Information Flow Technologies
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.

(in-package "COMMON-LISP-USER")

(defpackage :nst (:use :franz :cl))
(in-package :nst)

(asdf:defsystem :nst
    :serial t
    :components ((:file "package")
		 (:file "nst")))

;;;(asdf:defsystem :nst-test
;;;    :serial t
;;;    :depends-on (:nst)
;;;    :components ((:file "test-package")))

