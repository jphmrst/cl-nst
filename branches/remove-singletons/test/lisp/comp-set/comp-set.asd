(defpackage :comp-set-asd (:use :common-lisp :asdf))
(in-package :comp-set-asd)

(defsystem :comp-set
    :serial t
    :components ((:file "def") (:file "use")))
