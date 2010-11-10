;;; File globals.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010 Smart Information Flow Technologies.
;;; Written by John Maraist.
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
(in-package :defdoc)

(defvar *docstring-style* 'standard-docstring-style)

(defgeneric format-doc (stream style spec))

(defgeneric format-docspec (stream style spec type)
  (:method :around (stream style spec type)
     (cond ((symbolp style)
            (format-docspec stream (make-instance style)
                            spec type))
           (t (call-next-method))))
  (:method (stream style spec type)
     (format-docspec-element style type spec stream)))

(defgeneric format-docspec-element (style target-type element stream))
