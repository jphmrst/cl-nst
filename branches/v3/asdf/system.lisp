;;; File system.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2009 Smart Information Flow Technologies.
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
(in-package :sift.asdf-nst)


(defclass nst-testable (system)
     ((nst-packages
       :initarg :nst-packages
       :reader nst-packages
       :initform nil
       :documentation "Package whose def-test-groups are to be run."
       )
      (nst-package
       :initarg :nst-package
       :reader nst-package
       :initform nil
       :documentation "Packages whose def-test-groups are to be run."
       )
      (nst-group
       :initarg :nst-group
       :reader nst-group
       :initform nil
       :documentation "An NST test group, given as a dotted pair of a package
name plus the name of a test group in that package."
       )
      (nst-groups
       :initarg :nst-groups
       :reader nst-groups
       :initform nil
       :documentation "A list of NST test groups, each given as a dotted pair
of a package name plus the name of a test group in that package."
       )
      (nst-test
       :initarg :nst-test
       :reader nst-test
       :initform nil
       :documentation "A single NST test, given as a three-element list of a
package name, the test's group name, and the test name."
       )
      (nst-tests
       :initarg :nst-tests
       :reader nst-tests
       :initform nil
       :documentation "A list of NST tests, each given as a three-element list
of a package name, the test's group name, and the test name."))
  
  (:documentation "Class of ASDF systems that use NST for their test-op."))

(defmethod initialize-instance :after ((sys nst-testable)
				       &key &allow-other-keys)

  (when (and (or (nst-package sys)  (nst-group sys)  (nst-test sys))
	     (or (nst-packages sys) (nst-groups sys) (nst-tests sys)))
    (error "Do not mix single-item testing via :nst-package, :nst-group, \
:nst-test with multiple-item testing via :nst-packages, :nst-groups, \
:nst-tests"))
  
  (when (< 1 (+ (if (nst-package sys) 1 0)
		(if (nst-group sys) 1 0)
		(if (nst-test sys) 1 0)))
    (error "Do not use more than one of :nst-package, :nst-group, :nst-test \
\(use :nst-packages, :nst-groups, :nst-tests\)"))

  (when (or (nst-packages sys) (nst-groups sys) (nst-tests sys))
    (error
     "Not currently implemented: :nst-packages, :nst-groups, :nst-tests")))

(defmethod operation-done-p ((o asdf:test-op) (c nst-testable))
  "Always re-run NST."
  (values nil))

(defmethod perform ((o asdf:test-op) (c nst-testable))
  (with-accessors ((single-package nst-package)
		   (single-group nst-group)
		   (single-test nst-test)) c
    (cond 
      (single-package
       (nst:run-package single-package)
       (nst:report-package single-package))

      (single-group
       (let ((group-actual (intern (symbol-name (cdr single-group))
				   (find-package (car single-group)))))
	 (nst:run-group group-actual)
	 (nst:report-group group-actual)))

      (single-test
       (let ((group-actual (intern (symbol-name (cadr single-group))
				   (find-package (car single-group))))
	     (test-actual (intern (symbol-name (caddr single-group))
				  (find-package (car single-group)))))
	 (nst:run-test group-actual test-actual)
	 (nst:report-test group-actual test-actual))))))

