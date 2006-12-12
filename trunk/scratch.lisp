
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

:als nst
:pa nst
(def-fixtures f1 :bindings ((c 3) (d 'asdfg)))
(def-fixtures f2 :bindings ((d 4) (e 'asdfg)))
(defclass cc (test f1 f2) ())
(defparameter cco (make-instance 'cc))
(defmethod core ((o cc))
  (declare (special c) (special d) (special e))
  (format t "cc core~%") (format t "  ~s ~s ~s~%" c d e))
(run cco)

(def-fixtures f1 :bindings ((c 4) (d 'asdfg)))

:als nst
:pa nst
(def-fixtures f1 :bindings ((c 3) (d 'asdfg)))
(defmacro result-from-macro () nil)
(def-test-group g1 (f1)
  (def-test t1 :form (eql 1 1))
  (def-test t2 :form (eql 1 2))
  (def-test t3 :form (error "I give an error"))
  (def-test t4 :form (eql c 4))
  (def-test t5 :form (eq d 'asdfg))
  (def-test t6 :form (result-from-macro) :defer-compile t))
(def-fixtures f2 :uses (f1) :bindings ((d 4) (e 'asdfg) (f c)))
(def-test-group g2 (f2)
  (def-check ck1 symbol (intern "a") a)
  )
(defmacro result-from-macro () t)
(run-group 'g1)

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

;;; (def-fixtures f1 :bindings () ((c 3) (d 'asdfg)) ())
;;; (defun out-c () (declare (special c)) (format t "~s~%" c))
;;; (fwrap 'out-c 'wrapping 'f1)
;;; (out-c)
;;; (defun out-d () (declare (special d)) (format t "~s~%" d))
;;; (fwrap 'out-d 'wrapping 'f1)

;;; (def-test-group g1 (f1) (:setup (format t "yyy~%")))

(defmacro zz (za zb)
  (let ((z1 (gensym "z1-"))
	)
    `(progn
       (macrolet ((yy (ya yb)
		    (let ((y1 (gensym "y1-")))
		      `(let ((,y1 (+ 2 ya)))
			 (format t
				 "za ~d~%zb ~d~%ya ~d~%yb ~d~%y1 ~d~%"
				 ,,za ,,zb ,ya ,yb ,y1)))))
	 (yy 10 100)
	 (yy 20 200)))))

------------------------------------------------------------



;;;(defmacro for-permutations (id list not-first perm-so-far &rest forms)
;;;  
;;;  (cond
;;;    
;;;    ((null list)
;;;     (return-from for-permutations forms))
;;;    
;;;    ((eql 1 (length list))
;;;     (return-from for-permutations forms))
;;;  
;;;  )