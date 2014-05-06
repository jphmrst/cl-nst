;;; File junit.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Except as noted below, all contents are:
;;; Copyright (c) 2009-2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.
(in-package :sift.nst)

(defun nst-xml-dump (stream)
  (nst-junit-dump stream))

(defgeneric nst-junit-dump (stream))
(def-documentation (function nst-junit-dump)
  (:tags control)
  (:properties (api-summary control))
    (:blurb (:latex "Function \\texttt{nst-junit-dump} pushes the entire NST state to a JUnit XML file whose stream is specified by its argument.")))

(defmethod nst-junit-dump ((stream stream))
  (junit-header stream)
  ;; (junit-xml-snippet (all-groups-report) stream)
  (pprint-xml (all-groups-report) stream))

(defmethod nst-junit-dump ((filename string))
  "Write the results of this session's NST testing in JUnit XML format."
  (with-open-file (stream filename :if-exists :supersede
                          :if-does-not-exist :create)
    (nst-junit-dump stream)))

(defgeneric junit-group-result (group &rest args &key
                                dir file verbose stream
                                if-dir-does-not-exist if-file-exists))

(defmethod junit-group-result ((group symbol) &rest args &key
                               dir file verbose stream
                               if-dir-does-not-exist if-file-exists)
  (declare (ignorable dir file if-dir-does-not-exist if-file-exists stream)
           (ignore verbose))
  (apply #'junit-group-result (group-report group) args))

(defmethod junit-group-result ((group group-result) &rest args &key
                               verbose
                               (dir  nil dir-supp-p)
                               (file nil file-supp-p)
                               (stream nil stream-supp-p)
                               (if-dir-does-not-exist
                                :create if-dir-does-not-exist-supp-p)
                               (if-file-exists :new-version
                                               if-file-exists-supp-p))
  (declare (ignorable args))

  (when (and stream-supp-p (or dir-supp-p file-supp-p))
    (warn "Using :stream, ignoring :directory/:filename."))
  (when (and stream-supp-p (or if-dir-does-not-exist-supp-p
                               if-file-exists-supp-p))
    (warn "Options :if-dir-does-not-exist, :if-file-exists not used with :stream, ignoring"))

  (let ((the-stream
         (cond
          (stream-supp-p stream)

          ((or dir-supp-p file-supp-p)

           ;; If there's no filename, build a default one.
           (when (not file-supp-p)
             (setf file (parse-namestring (concatenate 'string
                                            "TEST-"
                                            (symbol-to-junit-name
                                             (group-result-group-name group))
                                            ".xml"))))

           ;; If there's no directory, use the current one.
           (when (not dir-supp-p)
             (setf dir (parse-namestring "./")))

           ;; If we just have a string for the filename, convert it to
           ;; a Lisp pathname.
           (when (and file-supp-p (stringp file))
             (setf file (parse-namestring file)))

           ;; If we just have a string for the directory, convert it
           ;; to a Lisp pathname.
           (when (and dir-supp-p (stringp dir))
             (setf dir (parse-namestring dir)))

           (let ((file-path (merge-pathnames file dir)))

             (when (eq if-dir-does-not-exist :create)
               (ensure-directories-exist file-path))

             (when verbose (format t "  Writing to ~w~%" file-path))
             (open file-path :direction :output :element-type :default
                   :if-exists if-file-exists :if-does-not-exist :create)))

          (t (setf stream-supp-p t)
             *standard-output*))))

    (junit-header the-stream)
    (pprint-xml group the-stream)
    (unless stream-supp-p (close the-stream))))

(defun junit-results-by-group (&rest args &key verbose &allow-other-keys)
  (loop for report in (multi-results-group-reports (all-groups-report)) do
    (when verbose
      (format t "Making XML for group ~s~%" (group-result-group-name report)))
    (apply #'junit-group-result report args)))
(def-documentation (function junit-results-by-group)
  (:tags control)
  (:properties (api-summary control))
    (:intro (:latex "The \\texttt{junit-results-by-group} function writes the NST test results in JUnit XML format, organized by group, aligning test groups with Java classes, and individual tests with \\texttt{@Test} methods."))
    (:callspec (&key (verbose flag) (dir directory) (file filespec)
                     (stream stream)
                     (if-dir-does-not-exist bool)
                     (if-file-exists bool)))
    (:details (:latex "Either \\texttt{:dir} and \\texttt{:file} options, or the \\texttt{:stream} option, but not both, should be used to specify the target for XML output; if none of the three options are given, the function will write to \\texttt{*standard-output*}.")))

;;; The following three definitions are
;;; Copyright (c) 2003, Miles Egan. All rights reserved.
;;; Modifications Copyright (c) 2011, John Maraist. All rights reserved.

;;; Redistribution and use [of these three definitions] in source and
;;; binary forms, with or without modification, are permitted provided
;;; that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials provided
;;;       with the distribution.
;;;
;;;     * The name of the author may not be used to endorse or promote
;;;       products derived from this software without specific prior
;;;       written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.


(defvar *entities*
  #(("lt;" #\<)
    ("gt;" #\>)
    ("amp;" #\&)
    ("apos;" #\')
    ("quot;" #\")))

(defvar *char-escapes*
  (let ((table (make-array 256 :element-type 'string :initial-element "")))
    (declare (type vector *entities*))
    (loop
     for code from 0 to 255
     for char = (code-char code)
     for entity = (first (find char *entities* :test #'char= :key #'second))
     do (setf (svref table code)
              (cond
                (entity
                 (concatenate 'string "&" entity))
                ((and (or (< code 32) (> code 126))
                      (not (= code 10))
                      (not (= code 9)))
                 (format nil "&#x~x;" code))
                (t
                 (format nil "~x" char))))
     finally (return table))
    table))

(defun string-escaped (string)
  (when (symbolp string)
    (return-from string-escaped (string-escaped (symbol-name string))))
  (with-output-to-string (stream)
    "Writes string to stream with all character entities escaped."
    #-allegro (coerce string 'simple-base-string)
    (loop for char across string
          for esc = (svref *char-escapes* (char-code char))
          do (write-sequence esc stream))))

;;; The above three definitions are Copyright (c) 2003, Miles Egan
;;; All rights reserved.
;;; Modifications Copyright (c) 2011, John Maraist. All rights reserved.
;;; See conditions and disclaimer above.
