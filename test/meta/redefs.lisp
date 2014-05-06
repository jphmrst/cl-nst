;;; File redefs.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2014 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
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

(in-package :mnst)

(def-criterion-alias (:redef)
  (let ((group-name (gentemp))
        (test-name (gentemp)))
    `(:progn
       (eval '(def-test-group ,group-name () (def-test ,test-name (:eql 6) 5)))
       (run-group ',group-name)
       (eval '(def-test-group ,group-name () (def-test ,test-name (:eql 5) 5)))
       (--nst-group ,group-name (---test-passes ,group-name ,test-name)))))

(def-criterion-alias (:redefN first-tests second-tests)
  (let ((group-name (gentemp)))
    `(:progn
       (eval '(def-test-group ,group-name ()
                ,@(loop for name in first-tests collect `(def-test ,name (:eql 6) 5))))
       (run-group ',group-name)
       (eval '(def-test-group ,group-name ()
                ,@(loop for name in second-tests collect `(def-test ,name (:eql 5) 5))))
       (:all ,@(loop for name in second-tests
                     collect `(--nst-group ,group-name (---test-passes ,group-name ,name)))
             ,@(loop for name in first-tests
                     if (not (member name second-tests))
                     collect `(--nst-no-test-in-group ,group-name ,name))))))

(def-test-group redefs ()
  (def-test redef0 :redef)
  (def-test redef1 (:redefN (aa bb cc) (xx yy))))
