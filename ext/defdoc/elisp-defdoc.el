
;;; Sample of what you might put in your .xemacs/init.el:

;; Indentation documentation at
;; http://tonic.physics.sunysb.edu/docs/emacs/xemacs.html#SEC191 .

(defmacro defdoc-indentation (name args)
  `(progn
     (put ',name 'lisp-indent-function ,args)
     (put ',name 'fi:lisp-indent-hook  ,args)))

(defdoc-indentation :docstring-installer 1)
(defdoc-indentation def-documentation 1)
(defdoc-indentation defdoc:def-documentation 1)
(defdoc-indentation def-element 3)
(defdoc-indentation defdoc:def-element 3)
(defdoc-indentation with-unpacked-standard-spec 2)
(defdoc-indentation with-possibly-unbound-slotaccessors 2)
(defdoc-indentation with-conjunctive-options 2)

