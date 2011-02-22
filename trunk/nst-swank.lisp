;;;---------------------------------------------------------------------------
;;; Copyright Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; SIFT PROPRIETARY
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    CL side of the NST SLIME interface...
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2011/02/21:rpg] Created.  FIXME: license and copyright should be fixed.
;;;
;;;---------------------------------------------------------------------------

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