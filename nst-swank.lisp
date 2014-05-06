;;; File nst-swank.lisp
;;;
;;;    CL side of the NST SLIME interface.
;;;
;;; Copyright (c) 2012 Robert Goldman
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

(in-package :swank)

;;; source for cargo culting
#|
(defslimefun set-package (name)
  "Set *package* to the package named NAME.
Return the full package-name and the string to use in the prompt."
  (let ((p (guess-package name)))
    (assert (packagep p) nil "Package ~a doesn't exist." name)
    (setq *package* p)
    (list (package-name p) (package-string-for-prompt p))))
|#

(defslimefun nst-run-package (name)
  "Try running the NST tests in package."
  (let ((p (guess-package name)))
    (assert (packagep p) nil "Package ~a doesn't exist." name)
    (nst-control-api:run-package p)
    (nst-control-api:report-package p)))