;;; File plaintext.lisp
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

(defclass standard-docstring-style () ())
(defmethod format-docspec (stream (style standard-docstring-style)
                                  spec type)
  (format stream "~{~a~^~%~}" (spec-to-lines style type spec 79)))

;;; -----------------------------------------------------------------

(defgeneric callspec-prefix (style target-type spec width calling)
  (:method (style target-type spec width (calling string))
     (declare (ignore style target-type spec width))
     (cond
       ((< 0 (length calling))
        (concatenate 'string "(" calling " "))
       (t calling)))
  (:method (style target-type spec width (calling symbol))
     (callspec-prefix style target-type spec width
                      (concatenate 'string
                                   (if (eq (symbol-package calling)
                                           (find-package :keyword))
                                     ":" "")
                                   (symbol-name calling))))
  (:method (style target-type spec width (calling null))
     (declare (ignore style target-type spec width))
     ""))

(defgeneric callspec-suffix (style target-type spec width calling)
  (:method (style target-type spec width calling)
     (declare (ignore style target-type spec width))
     (cond
       ((null calling) "")
       (t ")"))))

(defvar *tags* 0)
(defgeneric callspec-to-lines (style target-type spec width &optional calling)
  (:method (style target-type (spec standard-callspec) width
                  &optional (calling nil))
     (let* ((prefix (callspec-prefix style target-type spec width calling))
            (suffix (callspec-suffix style target-type spec width calling))
            (prefix-len (length prefix))) ;; ((*tags* (+ 1 *tags*)))

       (with-accessors ((mandatory mandatory)
                        (optional optional)
                        (keyword key)
                        (keyword-supp key-supp)
                        (body body))
           spec
         (setf mandatory
               (nconc mandatory
                      (loop for opt in optional collect `(:opt ,opt))))

         (let* ((man-lines
                 (when mandatory
                   (flow #'callspec-item-to-lines
                         style target-type mandatory (- width prefix-len
                                                               1))))

                (key-lines
                 (when keyword-supp
                   (flow #'keyargspec-to-lines style target-type
                         keyword (- width prefix-len 1))))

                (regular-arg-lines (append man-lines key-lines))

                (body-arg-lines
                 (loop for spec in body
                       append (callspec-item-to-lines style target-type
                                       spec (- width 3) t))))

           (cond
            ((and regular-arg-lines body-arg-lines)
             (bracket-with (append (bracket-with regular-arg-lines prefix nil)
                                   (indent-by body-arg-lines 2))
                           "" suffix))

            (regular-arg-lines
             (bracket-with regular-arg-lines prefix suffix))

            ((not body-arg-lines)
             (list (concatenate 'string prefix suffix)))

            ((eql (length calling) 0)   ; (and body-arg-lines ...)
             (bracket-with body-arg-lines prefix suffix))

            (t                          ; (and body-arg-lines (not ABOVE))
             (cons prefix
                   (indent-by (bracket-with body-arg-lines "" suffix)
                              2))))))))
  (:method (style target-type (spec macrolist-callspec) width
                  &optional calling)
     (declare (ignore style target-type width calling))
     (bracket-with (call-next-method) "(" ")" t)))

(defun callspec-items-to-alts (style target-type items max stack)
  (cond
   (stack
    (let ((first-item-lines (callspec-item-to-lines style target-type
                                                    (car items) max nil))
          (rest-item-lines (loop for i in (cdr items)
                               append (bracket-with (callspec-item-to-lines
                                                     style target-type
                                                     i (- max 3) nil)
                                              " | " ""))))
      (append first-item-lines rest-item-lines)))
   (t (flow #'callspec-item-to-lines
            style target-type
            `(,(car items) ,@(loop for i in (cdr items)
                                   append (list (intern " | ") i))) max))))

(defgeneric callspec-item-to-lines (style target-type item max &optional stack)
  (:method (style target-type (item symbol) max &optional stack)
     (declare (ignore style target-type stack max))
     (list (symbol-name item)))
  (:method (style target-type (item callspec-sequence-of) max &optional stack)
     (cond
       (stack (let ((block-lines (flow #'callspec-item-to-lines
                                        style target-type
                                        (repeated item) max)))
               (append block-lines (cons "..." block-lines))))
       (t (flow #'callspec-item-to-lines
                style target-type
               `(,@(repeated item) ,(intern "...") ,@(repeated item))
               max))))
  (:method (style target-type (item callspec-one-of) max &optional stack)
     (with-accessors ((items items)) item
       (bracket-with (callspec-items-to-alts style target-type items max stack)
                     "[ " " ] ")))
  (:method (style target-type (item callspec-bag-of) max &optional stack)
     (with-accessors ((items items)) item
       (bracket-with (callspec-items-to-alts style target-type items max stack)
                     "[ " " ]* ")))
  (:method (style target-type (item callspec-optional) max &optional stack)
     (declare (ignore stack))
     (let* ((items (callspec-item-to-lines style target-type
                                           (car (option item)) (- max 4))))
       (bracket-with items "[ " " ]")))
  (:method (style target-type (item callspec-keyheaded) max &optional stack)
     (declare (ignore stack))
     (with-accessors ((key-head key) (args forms)) item
       (let* ((key-head-string (format nil "(:~a " key-head))
              (items (flow #'callspec-item-to-lines style target-type args
                           (- max (length key-head-string) 1))))
         (bracket-with items key-head-string ")"))))
  (:method (style target-type (item standard-callspec) max &optional stack)
     (declare (ignore stack))
     (callspec-to-lines style target-type item max))
  (:method (style target-type item max &optional stack)
     (declare (ignore style target-type stack max))
     (error "Unrecognized callspec item ~w" item)))

(defun keyargspec-to-lines (style target-type spec max)
  (let* ((key (key spec))
         (prefix (concatenate 'string "[ :" (symbol-name key) " "))
         (form (arg spec)))
    (bracket-with (callspec-item-to-lines style target-type form
                                          (- max 2 (length prefix)))
                  prefix " ]")))

(defun fill-words (word-list length &optional (first-line-prefix ""))
  (let ((lines-bank nil)
        (current-line first-line-prefix)
        (current-line-length (length first-line-prefix)))

    ;; Loop through the words and form lines.
    (loop for word in word-list do
      (let ((word-length (length word)))
        (cond
          ;; If there's nothing on the current line, add the word to it.
          ((eql current-line-length 0)
           (setf current-line word current-line-length word-length))

          ;; If there's already something on the current line and
          ;; adding the word would blow the margin, then put the word
          ;; on a new line.
          ((>= (+ word-length 1 current-line-length) length)
           (push current-line lines-bank)
           (setf current-line word current-line-length word-length))

          ;; Otherwise extend the current line.
          (t
           (incf current-line-length (+ 1 word-length))
           (setf current-line (concatenate 'string current-line " " word))))))

    ;; If there's no prefix and an empty list, then this condition
    ;; might trigger.
    (when (> (length current-line) 0)
      (push current-line lines-bank))

    ;; We're pushing lines as we build them, so reverse the list
    ;; before returning it to get the right order.
    (nreverse lines-bank)))

;;; -----------------------------------------------------------------

(defun spec-to-lines (style target-type spec width)
  (output-lines style target-type spec width))

(defgeneric output-lines (style target-type spec-head spec))

(defmethod output-lines (style target-type (doc standard-doc-spec) width)
  (with-unpacked-standard-spec (self intro intro-supp-p params params-supp-p
                                     blurb blurb-supp-p details details-supp-p
                                     callspec) doc
     (cond
      ((or intro-supp-p details-supp-p)
       (nconc
        (when intro-supp-p (spec-to-lines style target-type intro width))
        (loop for cspec in callspec
            append (cons ""
                         (loop for line
                               in (callspec-to-lines style target-type
                                           cspec (- width 2) self)
                             collect (concatenate 'string "  " line))))
        (when params-supp-p
          (nconc
           (loop for (name subspec) in params
               append
                 (list* ""
                        (format nil "  ~a" name)
                        (loop for line in (spec-to-lines style target-type
                                               subspec (- width 4))
                              collect (concatenate 'string "    " line))))
           (list "")))
        (when details-supp-p (spec-to-lines style target-type details width))))
      (blurb-supp-p
       (nconc
        (spec-to-lines style target-type blurb width)
        (loop for cspec in callspec
            append (cons ""
                         (loop for line
                             in (callspec-to-lines style target-type
                                            cspec (- width 2) self)
                             collect (concatenate 'string "  " line))))
        (when params-supp-p
          (loop for (name subspec) in params
              append
                (list* ""
                       (format nil "  ~a" name)
                       (loop for line in (spec-to-lines style target-type
                                              subspec (- width 4))
                           collect (concatenate 'string "    " line)))))))
      (params-supp-p
       (nconc
        (loop for cspec in callspec
            append (loop for line in (callspec-to-lines style target-type
                                            cspec (- width 2) self)
                       collect (concatenate 'string "  " line)))
        (loop for (name subspec) in params
            append
              (list* ""
                     (format nil "  ~a" name)
                     (loop for line in (spec-to-lines style target-type
                                            subspec (- width 4))
                         collect (concatenate 'string "    " line))))))
      (t nil))))

(defmethod output-lines (style target-type (doc standard-plain-text) width)
  (declare (ignore style target-type))
  (let ((string (text-element-text doc)))
    (loop while (< 0 (length string))
          for (last-of-first first-of-rest)
            = (get-first-break-edges string width)
          if (<= (length string) width)
            collect string into lines
            and do (setf string "")
          else
            collect (subseq string 0 last-of-first) into lines
            and do (setf string (subseq string first-of-rest))
          end
          finally (return-from output-lines lines))))

(defmethod output-lines (style target-type (doc standard-latex) width)
  (let ((orig-string (latex-element-latex doc)))
    (let ((string-chars (loop for spot from (- (length orig-string) 1) downto 0
                              collect (elt orig-string spot))))
      (output-lines style target-type (make-instance 'standard-plain-text
                      :text (reassemble-latex-strip string-chars))
                    width))))

(defun reassemble-latex-strip (input &aux output)
  (loop while input for this = (pop input) do
    (cond
      ;; It's a space.  If it follows whitespace throw it away; else
      ;; keep it.
      ((eql this #\Space)
       (unless (whitespace-p (car input))  (push this output)))

      ;; Other whitespace: convert to a space.
      ((whitespace-p this)
       (push #\Space input))

      ;; It's a backslash, and so is the prior character: that's a
      ;; line break; replace them with a single space.
      ((and (eql this #\\) (eql (car input) #\\))
       (pop input)
       (push #\Space input))

      ;; An isolated backslash
      ((eql this #\\)
       (setf output (check-macro output)))

      ;; Everything else goes onto the output.
      (t (push this output))))

  ;; Coerce that list to a string.
  (coerce (make-array (length output) :initial-contents output) 'string))

(defun check-macro (output-stack)
  (let (macro-name
        (scanner output-stack))
    (loop while (and scanner (alpha-char-p (car scanner))) do
      (push (pop scanner) macro-name))
    (setf macro-name (coerce (reverse macro-name) 'string))
    (loop while (and scanner (whitespace-p (car scanner))) do
      (pop scanner))
    (cond
      ((eql (car scanner) #\{)
       (pop scanner)
       (let ((buffer nil))
         (loop while (and scanner (not (eql (car scanner) #\}))) do
               (push (pop scanner) buffer))
         (pop scanner)
         (cond
          ((or (string= macro-name "index"))
           nil)
          (t
           (loop while buffer do
                 (push (pop buffer) scanner))))
         scanner))
      (t output-stack))))

(defmethod output-lines (style target-type (doc standard-paragraph-list) width)
  (let ((paragraph-specs (paragraphlist-element-items doc)))
    (loop for (spec . other-specs) on paragraph-specs
        append (nconc (spec-to-lines style target-type spec width)
                      (when other-specs (list ""))))))

(defmethod output-lines (style target-type (doc standard-sequence) width)
  (let ((specs (sequence-element-items doc)))
    (loop for spec in specs
          append (spec-to-lines style target-type spec width))))

(defmethod output-lines (style target-type (doc standard-code) width)
  (declare (ignore style target-type width))
  (let ((string (code-element-string doc)))
    (let ((scan 0) (max (length string)))
      (loop for point = (position #\Newline string :start scan)
          while point
          collect (prog1 (subseq string scan point)
                    (setf scan (+ 1 point)))
          into lines
          finally
            (return-from output-lines
              (cond
                ((< scan max)
                 (nconc lines (list (subseq string scan))))
                (t lines)))))))

(defmethod output-lines (style target-type (doc standard-itemize) width)
  (let ((options (list-element-options doc))
                   (items (list-element-specs doc)))
    (declare (ignore options))
    (loop for item in items
          for i from 1
          append (loop for block-line in (spec-to-lines style target-type
                                              item (- width 4))
                       for j from 0
                       collect (concatenate 'string
                                 (if (> j 0) "    " "  * ")
                                 block-line)))))

(defmethod output-lines (style target-type (doc standard-enumerate) width)
  (let ((options (list-element-options doc))
                   (items (list-element-specs doc)))
    (declare (ignore options))
    (loop for item in items for i from 1
          append (let* ((prefix (format nil " ~d. " i))
                        (padding (make-string (length prefix)
                                              :initial-element #\Space))
                        (this-block (spec-to-lines style target-type item
                                                   (- width (length prefix)))))
                   (loop for block-line in this-block for j from 0
                         collect (concatenate 'string
                                   (if (> j 0) padding prefix)
                                   block-line))))))

(defun whitespace-p (char)
  (case char
    ((#\Space #\Tab #\Newline \#Return \#Linefeed \#Page) t)
    (otherwise nil)))

(defun get-first-break-edges (string width)
  (let ((len (length string)))
    (cond
      ((< width len)
       (let* ((rightmost-space
               (block find-last-space
                 (loop for cand from width downto 0
                       if (whitespace-p (elt string cand))
                         do (return-from find-last-space cand))
                 (loop for cand from (+ 1 width) upto (- len 1)
                       if (whitespace-p (elt string cand))
                         do (return-from find-last-space cand))
                 len))
              (last-nonspace
               (block find-last-nonspace
                 (loop for cand from (- rightmost-space 1) downto 0
                       if (not (whitespace-p (elt string cand)))
                         do (return-from find-last-nonspace cand))
                 -1)))
         (list (+ 1 last-nonspace) (+ 1 rightmost-space))))
      (t
       (list width width)))))