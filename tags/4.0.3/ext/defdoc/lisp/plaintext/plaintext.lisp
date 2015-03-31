;;; File plaintext.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; DefDoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with DefDoc.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.

(in-package :defdoc-plaintext)

(defmethod write-output ((style plaintext-style) output-name directory file-name
                         &rest keyargs &key
                         (echo (named-function write-plaintext-output-nop
                                 (lambda ()))) &allow-other-keys)
  (let ((output-framework (make-instance output-name)))
    (unless output-framework
      (error "No such output framework ~s" output-name))
    (funcall echo)
    (let* ((extension (apply #'get-filename-extension
                             style output-name directory file-name keyargs))
           (file-spec (merge-pathnames (concatenate 'string file-name extension)
                                       directory)))
      (with-open-file (out file-spec :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
        (apply #'format-doc out style output-framework keyargs)))))

(defmethod get-filename-extension ((style plaintext-style)
                                   output-name directory file-name-root
                                   &key &allow-other-keys)
  (declare (ignore output-name directory file-name-root))
  ".txt")

;;; -----------------------------------------------------------------

(defmethod format-doc (stream (style plaintext-style) doc
                              &key (width 78) &allow-other-keys)
  (format stream "~{~a~%~}" (output-lines style t doc width)))

(defmethod format-docspec (stream (style plaintext-style)
                                  (spec doc-spec) target-type
                                  &key (width 78) &allow-other-keys)
  (format stream "~{~a~%~}" (output-lines style target-type spec width)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod format-docspec-element ((style plaintext-style)
                                   type spec stream
                                   &key (width 80) &allow-other-keys)
  (pprint-logical-block (stream (output-lines style type spec width))
    (loop for line = (pprint-pop) while line do
      (princ line stream)
      (pprint-exit-if-list-exhausted)
      (pprint-newline :mandatory stream))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod format-standard-docspec-literal-text ((style plaintext-style)
                                                 text stream
                                                 &key &allow-other-keys)
  (princ text stream))

(defmethod format-output-contents-sep ((style plaintext-style)
                                       stream output i1 i2
                                       &key &allow-other-keys)
  (declare (ignore output i1 i2))
  (format stream "~%")
  (format stream "~%"))

(defmethod format-output-leader-material ((style plaintext-style)
                                          stream output &key &allow-other-keys)
  (declare (ignore output))
  (princ " " stream)
  (pprint-logical-block (stream '(1)) (call-next-method))
  (format stream "~%"))

(defmethod format-output-leader-title :after ((style plaintext-style)
                                              stream output
                                              &key &allow-other-keys)
  (declare (ignore output))
  (format stream "~%"))

;;; -----------------------------------------------------------------
