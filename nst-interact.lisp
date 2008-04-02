;;; File nst-interact.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with NST.  If not, see <http://www.gnu.org/licenses/>.
(in-package :sift.nst-interact)

(def-test-group interact-b ()
  (def-test fix0 :form (eql 1 1))
  (def-test broke :form (eql 1 2))
  (def-test fix1 :form (eql 2 2)))

(def-test-group interact-e ()
  (def-test fix0 :form (eql 1 1))
  (def-test broke :form (error "boom"))
  (def-test fix1 :form (eql 2 2)))
