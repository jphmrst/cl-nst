;;; File interrupt.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
;;; Written by John Maraist.
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
(in-package :sift.nst)

(defconstant +keyboard-interrupt-class+
    #+allegro 'excl:interrupt-signal
    #+sbcl 'sb-sys:interactive-interrupt
    #-(or allegro sbcl) nil
  "Name of the class of error thrown when a keyboard interrupt is received,
or nil if this implementation of Lisp has no such class.")

(defmacro handler-bind-interruptable (handlers &body forms)
  "Like handler-bind \(and the arguments are similar\), but cancel any error-
handling if the error is related to a keyboard interrupt."

  ;; If we know the class of keyboard interrupts of this system:
  #+(or allegro sbcl)
  (let ((e (gensym)) (keyboard-error (gensym)))
    `(let ((,keyboard-error nil))
       (handler-bind
           ;; First catch the class of keyboard interrupts, in which
           ;; case we disable further error handling.
           ((,+keyboard-interrupt-class+ #'(lambda (,e)
                                             (declare (ignore ,e))
                                             (setf ,keyboard-error t)))

            ;; Now insert all the rest of the handlers, checking
            ;; first that it's not an interrupt.
            ,@(loop for (typ handler) in handlers
                  collect `(,typ #'(lambda (,e)
                                     (unless ,keyboard-error
                                       (funcall ,handler ,e))))))
         ,@forms)))

  ;; Otherwise, if we're not on a system that deals with keyboard
  ;; interrupts, then this just rewrites into a handler-bind.
  #-(or allegro sbcl)
  `(handler-bind ,handlers ,@forms))
