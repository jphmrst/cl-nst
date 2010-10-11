(in-package :defdoc)

;;; Decoding the :callspec forms.

(defun destructure-callspec (callspec)
  (let ((mandatory nil)
        (keyword nil)
        (optional nil)
        (body nil)
        (keyword-supp nil)
        (optional-supp nil)
        (body-supp nil)
        (phase 0)
        (last nil))
    (loop for form in callspec do
      (flet ((unphased ()
               (error "Found ~s in callspec~@[ after ~s~]" form last)))
        (macrolet ((else-unphased (cnd &body body)
                     `(cond
                       (,cnd ,@body)
                       (t (unphased)))))
          (cond
           ((eq form '&key)
            (else-unphased (eql phase 0) (setf phase 1 last form
                                               keyword-supp t)))

           ((eq form '&optional)
            (else-unphased (eql phase 0) (setf phase '&optional last form
                                               optional-supp t)))

           ((or (eq form '&rest) (eq form '&body))
            (else-unphased (and (numberp phase) (< phase 2))
                           (setf phase 2 last form body-supp t)))

           ((eq phase '&optional)  (push form optional))
           ((eql phase 0)          (push form mandatory))
           ((eql phase 1)          (push form keyword))
           ((eql phase 2)          (push form body))))))
    (values (nreverse mandatory)
            (nreverse optional) optional-supp
            (nreverse keyword)  keyword-supp
            (nreverse body)     body-supp)))
