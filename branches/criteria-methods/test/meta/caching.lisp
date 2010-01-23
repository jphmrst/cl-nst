;;; File caching.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2010 Smart Information Flow Technologies.
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

(in-package :nst-meta-sources)

(defparameter *nst-fc1* 0)
(defparameter *nst-fc2* 0)

(nst:def-fixtures fixtures-cache-all (:cache t)
  (a (progn (incf *nst-fc1*) 0))
  (b (progn (incf *nst-fc2*) 0)))

(nst:def-fixtures fixtures-cache-one ()
  ((:cache t) a (progn (incf *nst-fc1*) 0))
  (b (progn (incf *nst-fc2*) 0)))

(nst:def-fixtures fixtures-cache-override-t (:cache t)
  ((:cache nil) a (progn (incf *nst-fc1*) 0))
  (b (progn (incf *nst-fc2*) 0)))

(nst:def-fixtures fixtures-cache-none ()
  (a (progn (incf *nst-fc1*) 0))
  (b (progn (incf *nst-fc2*) 0)))
