
(in-package :nst-manual)

;;;(eval-when (:compile-toplevel :load-toplevel)
;;;  (format t "either/before~%")
;;;  (eval-when (:load-toplevel)
;;;    (let ((x 3))
;;;      (defmethod ntest ((c (eql 4))) x))
;;;    (let ((x 5))
;;;      (defmethod ntest ((c (eql 5))) x))
;;;    (format t "load~%"))
;;;  (eval-when (:compile-toplevel)
;;;    (format t "compile~%"))
;;;  (format t "either/after~%"))

(nst::def-group group1t ()
  (nst::def-check check1t (:pass) ()) (nst::def-check check2t (:pass) ()))
