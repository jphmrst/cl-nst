;;; File block.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; DefDoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with DefDoc.  If not, see
;;; <http://www.gnu.org/licenses/>.

(in-package :defdoc)

(defun indent-by (lines length)
  (indent-with lines (make-string length :initial-element #\Space)))

(defun indent-with (lines prefix &key skip-first skips)
  (when skip-first (push 0 skips))
  (loop for line in lines
        for i from 0
        collect
        (cond
          ((member i skips :test 'eql) line)
          (t (concatenate 'string prefix line)))))

(defun bracket-with (lines prefix suffix &optional (pad nil))
  (when lines
    (let ((old-first-line (pop lines)))
      (when (and pad (> (length prefix) 0) (> (length old-first-line) 0))
        (setf prefix (concatenate 'string prefix " ")))
      (let* ((first-line (concatenate 'string prefix old-first-line))
             (indented-lines (indent-by lines (length prefix)))
             (result (cons first-line indented-lines)))
        (when (> (length suffix) 0)
          (let ((last-line-cons (last result)))
            (cond
             ((and pad (> (length (car last-line-cons)) 0))
              (setf (car last-line-cons)
                    (concatenate 'string (car last-line-cons) " " suffix)))
             (t (setf (car last-line-cons)
                      (concatenate 'string (car last-line-cons) suffix))))))
        result))))

(defun adjoin-blocks (block1 block2)
  (case (length block1)
    ((0) block2)
    ((1) (bracket-with block2 (car block1) ""))
    (otherwise
     (let* ((prefix (car (last block1)))
            (result (copy-seq block1))
            (cdr-holder (last result 2)))
       (setf (cdr cdr-holder) (bracket-with block2 prefix ""))
       result))))

(defun width (lines)
  (loop for line in lines maximizing (length line)))

(defun flow (formatter style target-type artifacts max)
  (let ((results nil))
    (loop while (and artifacts (null results)) do
      (setf results (nreverse (funcall formatter style target-type
                                       (pop artifacts) max))))
    (when artifacts
      (loop for art in artifacts do
        (let* ((last (pop results))
               (full-block (funcall formatter style target-type art max))
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
                (let* ((short-block (funcall formatter style target-type
                                             art space-on-last))
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
