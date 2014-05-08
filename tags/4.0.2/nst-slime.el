;;; File nst-slime.el
;;;
;;;    SLIME functions to allow exercising the NST library
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2012-2014 Smart Information Flow Technologies.
;;; Written by Robert P. Goldman
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

(define-slime-contrib slime-nst
  "SLIME interface to the NST lisp unit testing facility.

This contrib implements a number of SLIME commands for invoking NST 
commands."
  (:authors "Robert P. Goldman")
  (:license "LLGPL")
  ;; cargo-culted from slime-repl
  ;; (:on-load
  ;;  (add-hook 'slime-connected-hook 'slime-repl-connected-hook-function)
  ;;  (setq slime-find-buffer-package-function 'slime-repl-find-buffer-package))
  ;; (:on-unload (slime-repl-remove-hooks))
  )

;;; here's the template...
'(defslime-repl-shortcut nil ("change-directory" "!d" "cd")
  (:handler 'slime-set-default-directory)
  (:one-liner "Change the current directory."))

'(defun slime-set-default-directory (directory)
  "Make DIRECTORY become Lisp's current directory."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (let ((dir (expand-file-name directory)))
    (message "default-directory: %s"
             (slime-from-lisp-filename
              (slime-repl-shortcut-eval `(swank:set-default-directory
                                          ,(slime-to-lisp-filename dir)))))
    (with-current-buffer (slime-output-buffer)
      (setq default-directory dir))))

(defslime-repl-shortcut nil ("nst-run-package" "NP" "NRP")
  (:handler 'slime-nst-run-package)
  (:one-liner "Run the NST tests in PACKAGE."))

;;; Cargo-culted from slime-repl-set-package...
(defun slime-nst-run-package (package)
  "Run the NST tests in PACKAGE."
  (interactive (list (let* ((p (slime-current-package))
                            (p (and p (slime-pretty-package-name p)))
                            (p (and (not (equal p (slime-lisp-package))) p)))
                       (slime-read-package-name "NST Package: " p))))
  (with-current-buffer (slime-output-buffer)
    (let ((previous-point (- (point) slime-repl-input-start-mark)))
      (destructuring-bind (name prompt-string)
          (slime-repl-shortcut-eval `(swank:nst-run-package ,package))
        (slime-repl-insert-prompt)
        (when (plusp previous-point)
          (goto-char (+ previous-point slime-repl-input-start-mark)))))))

(provide 'slime-nst)