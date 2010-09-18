
(in-package :defdoc)

(defun spec-to-text (spec)
  (format nil "~{~a~^~%~}" (spec-to-lines spec 79)))

;;; -----------------------------------------------------------------

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
(defvar *tags* 0)
(defun callspec-to-lines (spec width &optional (calling nil)
                          &aux (tag *tags*))
  (declare (ignorable tag))
  (let ((*tags* (+ 1 *tags*)))
    (when (and (stringp calling) (< 0 (length calling)))
      (setf calling (concatenate 'string calling " ")))
    (when (null calling)
      (setf calling ""))
    (when (symbolp calling)
      (setf calling (concatenate 'string (symbol-name calling) " ")))

    (let ((prefix-len (+ 1 (length calling))))
      (multiple-value-bind (mandatory optional optional-supp
                            keyword keyword-supp body body-supp)
          (destructure-callspec spec)
        (declare (ignorable body-supp optional-supp))
        (setf mandatory (nconc mandatory
                               (loop for opt in optional collect `(:opt ,opt))))

        (let* ((man-lines
                (when mandatory
                  (flow #'callspec-item-to-lines mandatory (- width prefix-len
                                                              1))))

               (key-lines
                (when keyword-supp
                  (flow #'keyargspec-to-lines keyword (- width prefix-len 1))))

               (regular-arg-lines (append man-lines key-lines))

               (body-arg-lines
                (loop for spec in body
                    append (callspec-item-to-lines spec (- width 3) t)))

               (prefix (concatenate 'string "(" calling)))

          (cond
           ((and regular-arg-lines body-arg-lines)
            (bracket-with (append (bracket-with regular-arg-lines prefix nil)
                                  (indent-by body-arg-lines 2))
                          "" ")"))

           (regular-arg-lines
            (bracket-with regular-arg-lines prefix ")"))

           ((not body-arg-lines)
            (list (concatenate 'string prefix ")")))

           ((eql (length calling) 0)   ; (and body-arg-lines ...)
            (bracket-with body-arg-lines prefix ")"))

           (t                           ; (and body-arg-lines (not ABOVE))
            (cons prefix
                  (indent-by (bracket-with body-arg-lines "" ")") 2)))))))))

(defun callspec-item-to-lines (item max &optional stack)
  (let ((result
         (cond
          ((symbolp item)
           (list (symbol-name item)))

          ((and (listp item) (eq (car item) :seq))
           (cond
             (stack (let ((block-lines (flow #'callspec-item-to-lines
                                             (cdr item) max)))
                      (append block-lines (cons "..." block-lines))))
             (t (flow #'callspec-item-to-lines
                      `(,@(cdr item) ,(intern "...") ,@(cdr item))
                      max))))

          ((and (listp item) (eq (car item) :key-head))
           (destructuring-bind (token key-head . args) item
             (declare (ignore token))
             (let* ((key-head-string (format nil "(:~a " key-head))
                    (items (flow #'callspec-item-to-lines args
                                 (- max (length key-head-string) 1))))
               (bracket-with items key-head-string ")"))))

          ((and (listp item) (eq (car item) :opt))
           (let* ((items (callspec-item-to-lines (cadr item) (- max 4))))
             (bracket-with items "[ " " ]")))

          ((listp item)
           (callspec-to-lines item max))

          (t (error "Unrecognized callspec item ~w" item)))))

    result))

(defun keyargspec-to-lines (spec max)
  (let* ((key (car spec))
         (prefix (concatenate 'string "[ :" (symbol-name key) " "))
         (form (cadr spec)))
    (bracket-with (callspec-item-to-lines form (- max 2 (length prefix)))
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

(defun spec-to-lines (spec width)
  (output-lines (car spec) (cdr spec) width))

(defgeneric output-lines (spec-head spec width))

(defmethod output-lines ((in (eql :spec)) spec-args width)
  (destructuring-bind (&key self
                            (intro nil intro-supp-p)
                            (params nil params-supp-p)
                            (short nil short-supp-p)
                            (full nil full-supp-p)
                            callspec
                            &allow-other-keys) spec-args
    (cond
     ((or intro-supp-p full-supp-p)
      (nconc
       (when intro-supp-p (spec-to-lines intro width))
       (loop for cspec in callspec
             append (cons ""
                          (loop for line
                                in (callspec-to-lines cspec (- width 2) self)
                                collect (concatenate 'string "  " line))))
       (when params-supp-p
         (nconc
          (loop for (name subspec) in params
              append
                (list* ""
                       (format nil "  ~a" name)
                       (loop for line in (spec-to-lines subspec (- width 4))
                             collect (concatenate 'string "    " line))))
          (list "")))
       (when full-supp-p (spec-to-lines full width))))
     (short-supp-p
      (nconc
       (spec-to-lines short width)
       (loop for cspec in callspec
             append (cons ""
                          (loop for line
                              in (callspec-to-lines cspec (- width 2) self)
                              collect (concatenate 'string "  " line))))
       (when params-supp-p
         (loop for (name subspec) in params
               append
               (list* ""
                      (format nil "  ~a" name)
                      (loop for line in (spec-to-lines subspec (- width 4))
                          collect (concatenate 'string "    " line)))))))
     (params-supp-p
      (nconc
       (loop for cspec in callspec
             append (loop for line in (callspec-to-lines cspec (- width 2) self)
                          collect (concatenate 'string "  " line)))
       (loop for (name subspec) in params
           append
             (list* ""
                    (format nil "  ~a" name)
                    (loop for line in (spec-to-lines subspec (- width 4))
                        collect (concatenate 'string "    " line))))))
     (t nil))))

(defmethod output-lines ((in (eql :plain)) spec-args width)
  (destructuring-bind (string) spec-args
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

(defmethod output-lines ((in (eql :latex)) spec-args width)
  (destructuring-bind (orig-string) spec-args
    (let ((string-chars (loop for spot from (- (length orig-string) 1) downto 0
                              collect (elt orig-string spot))))
      (output-lines :plain `(,(reassemble-latex-strip string-chars)) width))))

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

(defmethod output-lines ((in (eql :paragraphs)) spec-args width)
  (destructuring-bind (&rest paragraph-specs) spec-args
    (loop for (spec . other-specs) on paragraph-specs
        append (nconc (spec-to-lines spec width)
                      (when other-specs (list ""))))))

(defmethod output-lines ((in (eql :seq)) spec-args width)
  (destructuring-bind (&rest paragraph-specs) spec-args
    (loop for spec in paragraph-specs
        append (spec-to-lines spec width))))

(defmethod output-lines ((in (eql :code)) args width)
  (declare (ignore width))
  (destructuring-bind (string) args
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

(defmethod output-lines ((in (eql :itemize)) spec-args width)
  (destructuring-bind (options &rest items) spec-args
    (declare (ignore options))
    (loop for item in items
          for i from 1
          append (loop for block-line in (spec-to-lines item (- width 4))
                       for j from 0
                       collect (concatenate 'string
                                 (if (> j 0) "    " "  * ")
                                 block-line)))))

(defmethod output-lines ((in (eql :enumerate)) spec-args width)
  (destructuring-bind (options &rest items) spec-args
    (declare (ignore options))
    (loop for item in items for i from 1
          append (let* ((prefix (format nil " ~d. " i))
                        (padding (make-string (length prefix)
                                              :initial-element #\Space))
                        (this-block (spec-to-lines item
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

;;;(defgeneric output-text (spec-head spec))
;;;
;;;(defmethod output-text ((in (eql :spec)) spec-args)
;;;  (destructuring-bind (&key self
;;;                            (intro nil intro-supp-p)
;;;                            (params nil params-supp-p)
;;;                            (short nil short-supp-p)
;;;                            (full nil full-supp-p) &allow-other-keys) spec-args
;;;    (let ((result ""))
;;;      (cond
;;;        ((or intro-supp-p full-supp-p)
;;;         (when intro-supp-p
;;;           (setf result (spec-to-text intro)))
;;;         (when params-supp-p
;;;           (loop for (name subspec) in params do
;;;             (setf result (format nil "~a~%~%  ~a~%  ~a"
;;;                            result name (spec-to-text subspec)))))
;;;         (when full-supp-p
;;;           (setf result (format nil "~a~%~%~a~%"
;;;                          result (spec-to-text full)))))
;;;        (short-supp-p
;;;         (setf result (spec-to-text short))
;;;         (when params-supp-p
;;;           (loop for (name subspec) in params do
;;;             (setf result (format nil "~a~%~%  ~a~%  ~a~%"
;;;                            result name (spec-to-text subspec))))))
;;;        (params-supp-p
;;;         (loop for (name subspec) in params do
;;;           (setf result (format nil "~a~%~%  ~a~%  ~a~%"
;;;                          result name (spec-to-text subspec)))))
;;;        (t nil))
;;;      result)))
;;;
;;;(defmethod output-text ((in (eql :plain)) spec-args)
;;;  (car spec-args))

