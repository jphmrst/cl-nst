
;;; Sample of what you might put in your .xemacs/init.el:
;;;
;;; (add-hook 'fi:common-lisp-mode-hook 'nst-lisp-filecheck-hook)
;;; (add-hook 'fi:inferior-common-lisp-mode-hook 'nst-lisp-filecheck-hook)
;;;
;;; You might also change or add to nst-auto-mode-list depending on
;;; your local naming conventions.

(defvar nst-lisp-mode nil)
(defvar nst-lisp-mode-hook nil)
(defvar nst-lisp-mode-map (make-sparse-keymap "NST mode map"))
(defvar nst-mode-known-check-stub-elements
    '((":pass")
      (":fail" FORMAT ARGS)
      (":warn" FORMAT ARGS)
      (":eq" FORM)
      (":symbol" NAME)
      (":eql" FORM)
      (":equal" FORM)
      (":equalp" FORM)
      (":forms-eq")
      (":forms-eql")
      (":predicate" PRED)
      (":err")
      (":perf" :ms MILLIS :sec SECS :min MINS)
      (":not" CRITERION)
      (":all" CRITERION CRITERION CRITERION)
      (":any" CRITERION CRITERION CRITERION)
      (":apply" FN CRITERION)
      (":check-err" CRITERION)
      (":progn" FORM FORM FORM FORM CRITERION)
      (":proj" (I J K) CRITERION)
      (":each" CRITERION)
      (":seq" CRITERION CRITERION CRITERION)
      (":permute" CRITERION)
      (":across" CRITERIA)
      (":slots" CLAUSE CLAUSE CLAUSE)
      )
  "Used by nst-lisp-insert-checkform to complete forms.")
(defvar nst-mode-checkform-history nil)


(defun nst-lisp-mode (arg)
  (interactive "p")
  (setq nst-lisp-mode
	(if (null arg)
	    (not nst-lisp-mode)
	    (> (prefix-numeric-value arg) 0)))
  (when nst-lisp-mode
    (run-hooks 'nst-lisp-mode-hook)))

(or (assq 'nst-lisp-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(nst-lisp-mode " NST") minor-mode-alist)))


(or (assq 'nst-lisp-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'nst-lisp-mode nst-lisp-mode-map)
		minor-mode-map-alist)))

(defun nst-lisp-insert-fixture-set ()
  (interactive)
  (insert "\n(def-fixtures NAME
   :documentation nil
   :uses () ; Other fixture sets which this set requires.
   :outer () ; List of clauses to be declared outside of any
             ; let-expression containing these bindings
   :inner () ; List of clauses to be declared immediately inside any
             ; let-expression containing these bindings
   :bindings
   ())\n"))

(defun nst-lisp-insert-fixture ()
  (interactive)
  (insert "\n    (NAME FORM)\n"))

(defun nst-lisp-insert-test-set ()
  (interactive)
  (insert "\n(def-test-group NAME
    () ; Fixtures to be used in this test group. 
  ;; (:documentation "")
  ;; (:setup FORM)
  ;; (:cleanup FORM)
  ;; (:defer-compile nil)
  )\n"))

(defun nst-lisp-insert-check ()
  (interactive)
  (insert "\n(def-check (NAME :fixtures ()
                 ;; :setup FORM
                 ;; :cleanup FORM
            )
      CRITERION
    FORMS)\n"))

(defun nst-lisp-insert-checkform ()
  (interactive)
  (let ((form (completing-read "Check form: "
			       nst-mode-known-check-stub-elements
			       nil 1 "" 'nst-mode-checkform-history)))
    (save-excursion
      (insert
       (format "%s"
	   (cons form
		 (cdr (assoc form
			     nst-mode-known-check-stub-elements))))))
    (forward-char 1)
    (forward-sexp 1)
    (forward-char 1)))

(defvar nst-lisp-key-prefix '((control c) (control n)))
(defmacro define-nst-key (suffix command)
  `(define-key nst-lisp-mode-map [,@nst-lisp-key-prefix ,@suffix]
     ,command))

(define-nst-key ((control s) (control f))
    'nst-lisp-insert-fixture-set)
(define-nst-key ((control s) (control t)) 'nst-lisp-insert-test-set)
(define-nst-key ((control f)) 'nst-lisp-insert-fixture)
(define-nst-key ((control c)) 'nst-lisp-insert-check)
(define-nst-key ((control k)) 'nst-lisp-insert-checkform)

;;; -----------------------------------------------------------------

(defvar nst-auto-mode-list (list "^nst-.*\\.lisp$"
				 "-nst\\.lisp$"))
(defun nst-auto-mode-match-p ()
  (block nst-auto-mode-match-p
    (let* ((file-full-name (buffer-file-name (current-buffer)))
	   (file-local-name (file-name-nondirectory file-full-name)))
      (loop for pattern in nst-auto-mode-list
	    do (when (string-match pattern file-local-name)
		 (return-from nst-auto-mode-match-p t))))))

(defun nst-lisp-filecheck-hook ()
  "Add this hook to your Lisp major mode to decide whether the NST
minor mode should apply."
  (nst-lisp-mode 
   (block check-file
     (when (nst-auto-mode-match-p) (return-from check-file t))
     (return-from check-file nil))))
