;;; File runnable.lisp
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


(defgeneric all-nst-tested (nst-test-runner &optional all-packages all-groups
                                            all-tests-by-group)

  (:documentation "Given an ASDF system, return the packages, test groups and
individual tests used to test that system")

  (:method (nst-test-runner &optional
                            (all-packages (make-hash-table :test 'eq))
                            (all-groups (make-hash-table :test 'eq))
                            (all-tests-by-group (make-hash-table :test 'eq)))
     (declare (ignorable nst-test-runner))
     (values all-packages all-groups all-tests-by-group)))

(defparameter *intermediate-system* nil
  "Dynamically-scoped variable used to indicate whether one system is being
tested on behalf on another.  If non-nil, then reporting of a system\'s results
may be suppressed.")

