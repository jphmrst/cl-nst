;;; File nst.asd
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman
;;;
;;; Version 1.0, July 10, 2007
;;;
;;; NST is Copyright (c) 2006 Smart Information Flow Technologies
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.

(defpackage :nst-asd (:use :common-lisp :asdf))
(in-package :nst-asd)

(defclass nst-tester (system) ())

(defsystem :nst
    :serial t
    :depends-on (:jm-defs)
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
