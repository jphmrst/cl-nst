;;; File artifacts.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
;;; Copyright (c) 2015-2016 John Maraist
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
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

(defstruct result-stats "Statistics common to the different result summaries."
  (tests 0) (passing 0) (erring 0) (failing 0) (warning 0)
  (elapsed-time 0)
  (timestamp (multiple-value-list (get-decoded-time))))
;;; Recording of results.  We use a hash table here --- unlike the
;;; method-based recording of test symbols, we're not worried about
;;; straddling the compile/load/run-time borders for result recording.
;;;
(defvar +results-record+ (make-hash-table :test 'eq)
  "Results of test runs.")

;;;;; Extracting information for debugging.
;;;
;;;(defgeneric trace-fixture (fx)
;;;  (:documentation "Provide debugging information about a fixture.")
;;;  (:method (fx) (format t "No known fixture ~s~%" fx)))
;;;(add-class-name-instantiator-method trace-fixture)
;;;
;;;(defgeneric trace-group (gr)
;;;  (:documentation "Provide debugging information about a group.")
;;;  (:method (gr) (format t "No known group ~s~%" gr)))
;;;(add-class-name-instantiator-method trace-group)
;;;
;;;(defgeneric trace-test (gr ts)
;;;  (:documentation "Provide debugging information about a test.")
;;;  (:method (gr ts) (format t "No known test ~s in group ~s~%" ts gr)))

(defun trace-results ()
  "Internal debugging function: dump the results hash."
  (loop for ts being the hash-keys of +results-record+ using (hash-value rs) do
    (format t "~s -> ~s~%" ts rs)))

;;; ----------------------------------------------------------------------
;;; This rest of this file contains generic functions for which
;;; methods are defined by the macros for user-defined artifacts
;;; (fixtures, groups, tests).

;;; With the Feb. 2016 rewrite of the internals to stop using a
;;; class/mixin implementation for groups, tests and fixtures, all of
;;; this gets much, much simpler.

;;; Tracking what an artifact is by name.
;;;
(defpackage :nst-name-usage)
(defun artifact-recorder (symbol)
  (intern (symbol-name symbol) :nst-name-usage))
;;;(defun indexing-name (record)
;;;  (let ((base-name (base-name record)))
;;;    (when base-name (artifact-recorder base-name))))

(defstruct name-use
  "Record for tracking the artifacts which NST associates with a name."
  name fixtures groups tests package-p last-use)

(defvar +name-uses+ (make-hash-table :test 'eq)
  "Map from a symbol in the =:nst-name-usage= package to a =name-use= instance
decribing how a symbol of the same name is used for NST artifacts.")
(defun name-use (symbol &optional no-create)
  (let ((artifact-recorder (artifact-recorder symbol)))
    (multiple-value-bind (name-use exists-p)
        (gethash artifact-recorder +name-uses+)
      (unless (or exists-p no-create)
        (setf name-use (make-name-use :name artifact-recorder)
              (gethash artifact-recorder +name-uses+) name-use))
      name-use)))

(defun record-package-use (base-name)
  (setf (name-use-package-p
         (name-use (intern (package-name (symbol-package base-name))
                           :keyword)))
        t))

(defun record-name-use (record)
  "Store the use of a name (as given by a fixture-, group-, or test-record)
in the index."
  (let ((base-name (base-name record)))
    (record-package-use base-name)
    (with-accessors ((fixtures name-use-fixtures)
                     (groups name-use-groups)
                     (tests name-use-tests)) (name-use base-name)

      ;; Basic pattern is: remove anything from the list under the
      ;; actually-packaged symbol, and then push in the new record.
      (cond
        ((fixture-record-p record)
         (setf fixtures (delete (fixture-record-name record) fixtures
                                :test 'eq :key #'fixture-record-name))
         (push record fixtures))

        ((group-record-p record)
         (setf groups (delete (group-record-name record) groups
                              :test 'eq :key #'group-record-name))
         (push record groups))

        ((test-record-p record)
         (setf tests (delete (test-record-name record) tests
                             :test 'eq :key #'test-record-name))
         (push record tests)))))

  (values nil))

(defun note-name-invocation (what)
  (setf (name-use-last-use (name-use (base-name what)))
        (cond
          ((packagep what) :package)
          (t what)))
  (values))

(defun executable-uses (name)
  "Return the list of group- and test-records, and possibly also a Lisp package
object, corresponding to possible intended targets of an NST run."
  (with-accessors ((as-package name-use-package-p)
                   (groups name-use-groups)
                   (tests name-use-tests)) (name-use name)
    (let ((gt (append groups tests)))
      (if (and as-package (find-package name))
        (cons (find-package name) gt)
        gt))))

(defun all-uses (name)
  "Return the list of group- and test-records, and possibly also a Lisp package
object, corresponding to possible intended targets of an NST run."
  (with-accessors ((as-package name-use-package-p)
                   (fixtures name-use-fixtures)
                   (groups name-use-groups)
                   (tests name-use-tests)) (name-use name)
    (let ((gt (append fixtures groups tests)))
      (if (and as-package (find-package name))
        (cons (find-package name) gt)
        gt))))

;;; =================================================================
;;; Old stuff

(set-pprint-dispatch 'name-use
  (named-function name-use-pprint-dispatch
    (lambda (stream name-use &aux (item 0))
      (with-accessors ((name name-use-name)
                       (as-package name-use-package-p)
                       (fixtures name-use-fixtures)
                       (groups name-use-groups)
                       (tests name-use-tests)) name-use
        (let* ((entities (let ((non-packages (append fixtures groups tests)))
                           (cond
                             ((find-package name)
                              (cons (find-package name) non-packages))
                             (t non-packages)))))
          (cond
            (entities
             (let ((enumerate (and entities (cdr entities))))
               (loop for entity in entities do
                 (blurb-one-name-use stream name entity
                                                 (incf item) enumerate)
                 (format stream "~%"))))
            (t (format stream "The name ~a is not known to NST" name))))))))

(defun blurb-one-name-use (stream name entity item enumerate)
  (when enumerate (format stream "~d. " item))
  (cond
    ((packagep entity) (format stream "~a is a package" name))
    ((fixture-record-p entity)
     (format stream "~s is a fixture" (fixture-record-name entity)))
    ((group-record-p entity)
     (format stream "~s is a test group" (group-record-name entity)))
    ((test-record-p entity)
     (format stream "~s is a test in group ~s" (test-record-name entity)
             (group-record-name (test-record-group entity))))
    (t (format stream "~a" entity))))


