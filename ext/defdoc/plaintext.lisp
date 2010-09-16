
(in-package :defdoc)

(defun spec-to-text (spec)
  (format nil "~{~a~^~%~}" (spec-to-lines spec 78)))

(defgeneric output-text (spec-head spec))

(defmethod output-text ((in (eql :spec)) spec-args)
  (destructuring-bind (&key (intro nil intro-supp-p)
                            (params nil params-supp-p)
                            (short nil short-supp-p)
                            (full nil full-supp-p) &allow-other-keys) spec-args
    (let ((result ""))
      (cond
        ((or intro-supp-p full-supp-p)
         (when intro-supp-p
           (setf result (spec-to-text intro)))
         (when params-supp-p
           (loop for (name subspec) in params do
             (setf result (format nil "~a~%~%  ~a~%  ~a"
                            result name (spec-to-text subspec)))))
         (when full-supp-p
           (setf result (format nil "~a~%~%~a~%"
                          result (spec-to-text full)))))
        (short-supp-p
         (setf result (spec-to-text short))
         (when params-supp-p
           (loop for (name subspec) in params do
             (setf result (format nil "~a~%~%  ~a~%  ~a~%"
                            result name (spec-to-text subspec))))))
        (params-supp-p
         (loop for (name subspec) in params do
           (setf result (format nil "~a~%~%  ~a~%  ~a~%"
                          result name (spec-to-text subspec)))))
        (t nil))
      result)))

(defmethod output-text ((in (eql :plain)) spec-args)
  (car spec-args))

;;; -----------------------------------------------------------------

(defun spec-to-lines (spec width)
  (output-lines (car spec) (cdr spec) width))

(defgeneric output-lines (spec-head spec width))

(defmethod output-lines ((in (eql :spec)) spec-args width)
  (destructuring-bind (&key (intro nil intro-supp-p)
                            (params nil params-supp-p)
                            (short nil short-supp-p)
                            (full nil full-supp-p) &allow-other-keys) spec-args
    (cond
     ((or intro-supp-p full-supp-p)
      (nconc
       (when intro-supp-p (spec-to-lines intro width))
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
       (when params-supp-p
         (loop for (name subspec) in params
               append
               (list* ""
                      (format nil "  ~a" name)
                      (loop for line in (spec-to-lines subspec (- width 4))
                          collect (concatenate 'string "    " line)))))))
     (params-supp-p
      (loop for (name subspec) in params
            append
            (list* ""
                   (format nil "  ~a" name)
                   (loop for line in (spec-to-lines subspec (- width 4))
                       collect (concatenate 'string "    " line)))))
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