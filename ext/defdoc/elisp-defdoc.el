;;; File elisp-defdoc.el
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2014 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
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

