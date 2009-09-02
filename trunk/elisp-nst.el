
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
  "Minor mode for editing NST Lisp files.

Adds the following key commands:

 C-c C-n           Default common prefix for all nst-mode commands.  Can
                   be customized; see below.
 C-c C-n C-s       Prefix for all commands about sets
 
 C-c C-n C-s C-f   Insert a fixture set skeleton at the point.
 C-c C-n C-s C-t   Insert a test set skeleton at the point.
 C-c C-n C-f       Insert a fixture skeleton at the point.
 C-c C-n C-c       Insert a single check at the point.
 C-c C-n C-k       Insert a check criterion skeleton; you will be
                   prompted for the criterion name (e.g. :eq, :all)
                   and a skeleton with appropriate arguments will be
                   inserted at the point.

Customization variables: Use nst-lisp-key-prefix to change the prefix
for the key commands in this minor mode.  Note that this must be set
BEFORE the mode is loaded, e.g. in the .xemacs/init.el file.

Known shortcomings: 
 - The skeleton inserters aren't aware of the context of point: it would
   be nicer if they moved to the next appropriate place to make the
   insertion, for example.
"
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
    (:uses () ;; Other fixture sets which this set requires.
     :documentation nil
     :outer () ;; List of clauses to be declared outside of any
               ;; let-expression containing these bindings.
     :inner () ;; List of clauses to be declared immediately inside
               ;; any let-expression containing these bindings.
     )
  )\n"))

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
  (insert "\n\n  (def-check (NAME :fixtures ()
                 ;; :setup FORM
                 ;; :cleanup FORM
              )
      CRITERION
    FORMS)")
  (backward-sexp 1)
  (forward-char 1)
  (forward-sexp 1)
  (forward-char 2))

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
      (loop for pattern in nst-auto-mode-list do
	(when (string-match pattern file-local-name)
	  (return-from nst-auto-mode-match-p t)))
      (return-from nst-auto-mode-match-p nil))))

(defun nst-lisp-filecheck-hook ()
  "Add this hook to your Lisp major mode to decide whether the NST
minor mode should apply."
  (make-variable-buffer-local 'nst-lisp-mode)
  (nst-lisp-mode 
   (block check-file
     (when (nst-auto-mode-match-p) (return-from check-file t))
     (return-from check-file 0))))

;; Indentation documentation at
;; http://tonic.physics.sunysb.edu/docs/emacs/xemacs.html#SEC191 .

(defmacro nst-form-indentation (name args)
  `(progn
     (put ',name 'lisp-indent-function ,args)
     (put ',name 'fi:lisp-indent-hook  ,args)))

(nst-form-indentation def-fixtures 1)
(nst-form-indentation nst:def-fixtures 1)
(nst-form-indentation def-test-group 2)
(nst-form-indentation nst:def-test-group 2)
(nst-form-indentation def-check 2)
(nst-form-indentation nst:def-check 2)
(nst-form-indentation def-test 2)
(nst-form-indentation nst:def-test 2)
(nst-form-indentation def-check-alias 1)
(nst-form-indentation nst:def-check-alias 1)
(nst-form-indentation def-criterion-alias 1)
(nst-form-indentation nst:def-criterion-alias 1)
(nst-form-indentation def-value-check 1)
(nst-form-indentation nst:def-value-check 1)
(nst-form-indentation def-values-criterion 1)
(nst-form-indentation nst:def-values-criterion 1)
(nst-form-indentation def-control-check 1)
(nst-form-indentation nst:def-control-check 1)
(nst-form-indentation def-form-criterion 1)
(nst-form-indentation nst:def-form-criterion 1)
(nst-form-indentation continue-check 1)
(nst-form-indentation nst:continue-check 1)
(nst-form-indentation add-failure 1)
(nst-form-indentation nst:add-failure 1)
(nst-form-indentation add-error 1)
(nst-form-indentation nst:add-error 1)
(nst-form-indentation add-info 1)
(nst-form-indentation nst:add-info 1)
(nst-form-indentation at-verbosity 1)
(nst-form-indentation nst::at-verbosity 1)
(nst-form-indentation format-at-verbosity 2)
(nst-form-indentation nst::format-at-verbosity 2)

(nst-form-indentation :info 1)
(nst-form-indentation :apply 1)
(nst-form-indentation :check-err 1)
(nst-form-indentation :proj 1)
(nst-form-indentation :sample 0)
