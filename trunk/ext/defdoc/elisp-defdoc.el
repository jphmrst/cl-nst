
;;; Sample of what you might put in your .xemacs/init.el:

;; Indentation documentation at
;; http://tonic.physics.sunysb.edu/docs/emacs/xemacs.html#SEC191 .

(defmacro defdoc-indentation (name args package)
  (let ((qual-name (intern (concatenate 'string "defdoc:" (symbol-name name))))
	(dqual-name (intern (concatenate 'string
					 "defdoc-core:" (symbol-name name))))
	(cqual-name (intern (concatenate 'string "defdoc-control-api:"
					 (symbol-name name))))
	(result `((put ',name 'lisp-indent-function ,args)
		  (put ',name 'fi:lisp-indent-hook  ,args))))

    (cond 
      ((eq package :keyword)
       (push `(put ',name 'lisp-indent-function ,args) result)
       (push `(put ',name 'fi:lisp-indent-hook  ,args) result))
      ((eq package :defdoc)
       (push `(put ',qual-name 'lisp-indent-function ,args) result)
       (push `(put ',qual-name 'fi:lisp-indent-hook  ,args) result))
      ((eq package :defdoc-control-api)
       (push `(put ',cqual-name 'lisp-indent-function ,args) result)
       (push `(put ',cqual-name 'fi:lisp-indent-hook  ,args) result))
      ((or (null package) (eq package :defdoc-control-api))
       (push `(put ',dqual-name 'lisp-indent-function ,args) result)
       (push `(put ',dqual-name 'fi:lisp-indent-hook  ,args) result)))

    (cons 'progn result)))

(defdoc-indentation :paragraphs 0 :keyword)
(defdoc-indentation :itemize 1 :keyword)
(defdoc-indentation :enumerate 1 :keyword)
(defdoc-indentation :docstring-installer 1 :keyword)
(defdoc-indentation :defdoc 0 :keyword)

(defdoc-indentation def-documentation 1 :defdoc)
(defdoc-indentation def-element 3 :defdoc)
(defdoc-indentation def-output-framework 2 :defdoc)
(defdoc-indentation def-target-type 2 :defdoc)
(defdoc-indentation collect-groups-by-label 1 :defdoc)
(defdoc-indentation collect-output 2 :defdoc)
(defdoc-indentation collect-doc 1 :defdoc)
(defdoc-indentation collect-symbols 1 :defdoc)
(defdoc-indentation collect-docspec 2 :defdoc)

(defdoc-indentation with-unpacked-standard-spec 2 :defdoc-control-api)

(defdoc-indentation with-possibly-unbound-slotaccessors 2 nil)
(defdoc-indentation with-conjunctive-options 2 nil)
(defdoc-indentation def-slot-supp-predicates 1 nil)
(defdoc-indentation defdoc-debug 1 nil)
(defdoc-indentation def-bundle-package 2 nil)
(defdoc-indentation def-property-label 2 nil)
(defdoc-indentation def-label-config 1 nil)
(defdoc-indentation def-generic-fn-default-nil 2 nil)
(defdoc-indentation with-name-and-filters 1 nil)
(defdoc-indentation def-doc-tag 2 nil)

