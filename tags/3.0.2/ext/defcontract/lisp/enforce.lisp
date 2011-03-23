;;; File enforce.lisp
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
(in-package :defcontract)

(defmacro apply-contract (name &rest keyvals &key &allow-other-keys)
  `(run-contract-enforcement ',name ,@keyvals))
