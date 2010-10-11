
(in-package :defdoc)

(defun indent-by (lines length)
  (loop for line in lines
        collect (concatenate 'string
                  (make-string length :initial-element #\Space) line)))

(defun bracket-with (lines prefix suffix)
  (when lines
    (let* ((first-line (concatenate 'string prefix (pop lines)))
           (indented-lines (indent-by lines (length prefix)))
           (result (cons first-line indented-lines))
           (last-line-cons (last result)))
      (setf (car last-line-cons)
            (concatenate 'string (car last-line-cons) suffix))
      result)))

(defun width (lines)
  (loop for line in lines maximizing (length line)))

(defun flow (formatter artifacts max)
  (let ((results nil))
    (loop while (and artifacts (null results)) do
      (setf results (nreverse (funcall formatter (pop artifacts) max))))
    (when artifacts
      (loop for art in artifacts do
        (let* ((last (pop results))
               (full-block (funcall formatter art max))
               (last-needs-length (+ 1 (length last)))
               (space-on-last (- max last-needs-length)))
          (when full-block
            (let ((full-width (width full-block))
                  (full-height (length full-block)))
              (cond

               ((< full-width space-on-last)
                (push (concatenate 'string last " " (pop full-block)) results)
                (loop for line in (indent-by full-block last-needs-length) do
                  (push line results)))

               ((>= full-width max)
                (push last results)
                (loop for line in full-block do (push line results)))

               (t
                (let* ((short-block (funcall formatter art space-on-last))
                       (short-width (width short-block))
                       (short-height (length short-block)))
                  (cond

                   ((and (< short-width space-on-last)
                         (< short-height (* 2 full-height)))
                    (push (concatenate 'string last " " (pop short-block))
                          results)
                    (loop for line in (indent-by short-block last-needs-length)
                        do (push line results)))

                   (t ;; (< short-width space-on-last)
                    (push last results)
                    (loop for line in full-block do
                      (push line results))))))))))))
    (nreverse results)))

;;; -----------------------------------------------------------------
