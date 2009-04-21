
(defgeneric wrap (o)
  (:method (o) (format t "core~%")))

(defclass c1 () ())
(defmethod wrap :around ((o c1))
  (format t "c1 wrap~%")
  (call-next-method)
  (format t "c1 unwrap~%"))

(defclass c2 () ())
(defmethod wrap :around ((o c2))
  (format t "c2 wrap~%")
  (call-next-method)
  (format t "c2 unwrap~%"))

(defclass cc (c1 c2) ())
(defparameter cco (make-instance 'cc))

(defclass cc (c1 c2) ())
(defparameter cco (make-instance 'cc))

------------------------------------------------------------

;;;:als nst
;;;:pa nst
;;;
;;;(def-fixtures f1 ()
;;;  ((c 3)
;;;   (d 'asdfg))
;;;  ())
;;;
;;;(def-fixtures f1 ()
;;;  ((c 4)
;;;   (d 'asdfg))
;;;  ())
;;;
;;;(def-test-group g1 (f1)
;;;  (def-test t1 :form (eql 1 1))
;;;  (def-test t2 :form (eql 1 2))
;;;  (def-test t3 :form (eql 2 2))
;;;  (def-test t4 :form (eql c 3))
;;;  (def-test t5 :form (eq d 'asdfg)))
;;;
;;;(def-test-group g1 (f1)
;;;  (def-test t1 :form (eql 1 1))
;;;  (def-test t3 :form (eql 2 2))
;;;  (def-test t7 :form (eq d 'asdfg))
;;;  (def-test t8 :form (eql c 4)))
;;;
;;;:nst :dump
;;;
;;;:nst :run-test g1 t1
;;;
;;;:nst :run-group g1
;;;
;;;:nst help

------------------------------------------------------------

;;; (def-fixtures f1 () ((c 3) (d 'asdfg)) ())
;;; (defun out-c () (declare (special c)) (format t "~s~%" c))
;;; (fwrap 'out-c 'wrapping 'f1)
;;; (out-c)
;;; (defun out-d () (declare (special d)) (format t "~s~%" d))
;;; (fwrap 'out-d 'wrapping 'f1)

;;; (def-test-group g1 (f1) (:setup (format t "yyy~%")))
