;;;---------------------------------------------------------------------------
;;; Copyright Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; SIFT PROPRIETARY
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    SLIME functions to allow exercising the NST library
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2011/02/21:rpg] Created.  FIXME: license and copyright should be fixed.
;;;
;;;---------------------------------------------------------------------------

(define-slime-contrib slime-nst
  "SLIME interface to the NST lisp unit testing facility.

This contrib implements a number of SLIME commands for invoking NST 
commands."
  (:authors "Robert P. Goldman")
  (:license "GPL")
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