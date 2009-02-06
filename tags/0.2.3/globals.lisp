;;; File nst.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with NST.  If not, see
;;; <http://www.gnu.org/licenses/>.
(in-package :sift.nst)


;;; Some general code remarks:
;;;
;;;   1. There's probably some "hangover" code here having to do with
;;;   my unfamiliarity with some of the dark corners of macro
;;;   expansion.  One of these days, this could use a good,
;;;   comprehensive code review.
;;;
;;;   2. Should the nst-run-command function be turned into a macro?
;;;

;;; This file contains settings, global variables and flags.

;;; Options for output in the interactive system.
;;;
(defmacro def-flag (flag-name flag-value implying-flags
			      &key (documentation nil doc-sup-p)
			      runtime-macro function-name)
  `(progn
     (defparameter ,flag-name ,flag-value
       ,(when doc-sup-p documentation))
     ,(when function-name
	`(defun ,function-name () (or ,flag-name ,@implying-flags)))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       ,(when runtime-macro
	  `(defmacro ,runtime-macro (&rest forms)
	     (let ((name-echo ',flag-name)
		   (flags-echo ',implying-flags))
	       `(when (or ,name-echo ,@flags-echo) ,@forms)))))
     t))

(def-flag *debug-forms-manip* nil ()
	  :runtime-macro forms-dbg
	  :documentation "Set to t to generate debugging information \
                          about certain macro expansions")
(def-flag *debug-fixtures* nil ()
	  :runtime-macro fixture-dbg
	  :documentation "Set to t to generate debugging information \
                          about fixtures")
(def-flag *debug-class-hierarchy* nil ()
	  :runtime-macro class-dbg
	  :documentation "Set to t to generate debugging information \
                          about the class hierarchy of tests and \
                          groups")
(def-flag *debug-macrotime* nil
  (*debug-class-hierarchy* *debug-fixtures* *debug-forms-manip*)
	  :runtime-macro macro-dbg
	  :documentation "Set to t for extensive macro expansion \
                          debugging output")
(def-flag *debug-compile* nil (*debug-forms-manip* *debug-fixtures*)
	  :runtime-macro compile-dbg
	  :documentation "Set to t for extensive debugging output for \
                          expanded macros")
(def-flag *debug-bindings* nil (*debug-fixtures*)
	  :runtime-macro bind-dbg
	  :documentation "Set to t to generate debugging information \
                          about fixture bindings in tests and groups")
(def-flag *debug-output* nil (*debug-class-hierarchy* *debug-bindings*)
	  :runtime-macro run-dbg
	  :documentation "Set to t for extensive runtime debugging \
                          output")
(def-flag *verbose-output* nil (*debug-output*)
	  :function-name use-verbose-output
	  :runtime-macro verbose-out
	  :documentation "Set to t for verbose output during test\
                          execution.  This setting is implied by\
                          *debug-output*.")

(def-flag *scheduled-summary-output* t ()
	  :documentation "Set to t for summaries of runs of scheduled\
                          tests.")
(def-flag *scheduled-single-output* nil ()
	  :documentation "Set to t for summaries of single test, group\
                          or package runs.")
(def-flag *defer-test-compile* t ()
	  :documentation "Set to t to defer compilation of test forms\
                          until runtime.")

;;; The fixtures, groups and tests that have been defined.
;;;
(defvar +fixtures+ nil
  "For user echo of fixture forms and other debugging." )
(defvar +fixture-def-names+ (make-hash-table)
  "The names bound by each fixture.")
(defvar +group-def-names+ (make-hash-table)
  "The names bound by each group" )
(defvar +groups+ (make-hash-table)
  "Declared test NST groups")
(defvar +groups-by-package+ (make-hash-table)
  "Hash table from packages to sets of groups, that is, to
other hash tables each mapping group records to t or nil.")


;;; Options for breaking at failed and erroneous tests in the
;;; interactive system.
;;;
(defvar *break-on-wrong* nil
  "When set to t, directs the test runner to return to the command
line whenever a test does not succeed.")
(defvar *break-on-error* nil
  "When set to t, directs the test runner to return to the command
line whenever a test raises an error condition, rather than returning
a boolean value.")
(defvar *debug-on-error* nil
  "When set to t, directs the test runner to return in debugging mode
whenever a test raises an error condition, rather than returning a
boolean value.")

;;; Options for opening fictures into the interactive system.
;;;
(defvar *open-used-fixtures* t
  "If t, then (re-)opening a fixture will always (re-)open the fixtures
it uses.")
(defvar *reopen-fixtures* nil
  "If nil, then will will never open the same fixture twice.")
;;; The packages, groups and tests that have been marked as
;;; interesting for quick execution in the runtime system.
;;;
(defvar *interesting-packages* nil
  "The names of packages whose tests should be checked by the :run
command to the NST runtime system.")
(defvar *interesting-group-names* nil
  "The names of groups whose tests should be checked by the :run
command to the NST runtime system.")
(defvar *interesting-test-names* (make-hash-table)
  "The names of groups whose tests should be checked by the :run
command to the NST runtime system.  The hash is against first group
names, and then test names.")

(defmacro have-interesting-tests ()
  "Poll the above variables to check for interesting tests."
  `(or *interesting-packages*
       *interesting-group-names*
       (> (hash-table-count *interesting-test-names*) 0)))


;;; The packages, groups and tests that remain to be run in the
;;; current :run session.
;;;
(defvar *pending-packages* nil
  "The names of packages whose tests remain to be checked under the
current :run session of the NST runtime system.")
(defvar *pending-group-names* nil
  "The names of groups whose tests remain to be checked under the
current :run session of the NST runtime system.")
(defvar *pending-test-names* (make-hash-table)
  "The names of groups whose tests remain to be checked under the
current :run session of the NST runtime system.  The hash is against
first group names, and then test names.")
(defmacro have-pending-tests ()
  "Poll the above variables to check for pending tests."
  `(or *pending-packages*
       *pending-group-names*
       (> (hash-table-count *pending-test-names*) 0)))

(defparameter *passed-test-count* 0
  "The number of tests passed under the current :run session of the NST
runtime system.")

;;; The groups and tests that failed or caused an error in the current
;;; :run session.
;;;
(defparameter *erred-groups* (make-hash-table)
  "Map from groups raising an error in setup during the current
:run session of the NST runtime system to a reason for the error,
or t if none is available.")
(defparameter *erred-cleanup* (make-hash-table)
  "Map from groups raising an error in cleanup during the current
:run session of the NST runtime system to a reason for the error,
or t if none is available.")

;;; These maps and sets are implemented as double-hash tables, and
;;; managed by the macros below.
;;;
(defparameter *passed-tests* (make-hash-table)
  "Set of tests which passed on their most recent run.")
(defparameter *failed-tests* (make-hash-table)
  "Hash table that maps group names to another hash which represents a
set of tests in that group failing on their most recent run.")
(defparameter *erred-tests* (make-hash-table)
  "Map from tests raising an error condition during the current :run
session of the NST runtime system to a reason for the error, or t if
none is available.")

(defmacro if-test (storage group-name test-name)
  "Where storage is some double hash table, return what, if anything,
is stored against group-name and test-name."
  (let ((group-hash (gensym "group-hash-")))
    `(let ((,group-hash (gethash ,group-name ,storage)))
       (when ,group-hash
	 (gethash ,test-name ,group-hash)))))

(defmacro clear-test (storage group-name test-name)
  "Remove anything stored against group-name and test-name in the given
double-hash table."
  (let ((group-hash (gensym "group-hash-")))
    `(let ((,group-hash (gethash ,group-name ,storage)))
       (when ,group-hash
	 (remhash ,test-name ,group-hash)))))

(defmacro add-test (test-record-hash test-record &optional (value t))
  (let ((group-hash (gensym "group-hash")))
    `(with-slots (group test-name) ,test-record
       (with-slots (group-name) group
	 (let ((,group-hash (gethash group-name ,test-record-hash)))
	   (unless ,group-hash
	     (setf ,group-hash (make-hash-table)
		   (gethash group-name ,test-record-hash) ,group-hash))
	   (setf (gethash test-name ,group-hash) ,value))))))

(defmacro have-erred-tests ()
  "Poll the above variables to check for erred tests."
  `(or (> (hash-table-count *erred-groups*) 0)
       (> (hash-table-count *erred-cleanup*) 0)
       (> (hash-table-count *failed-tests*) 0)
       (> (hash-table-count *erred-tests*) 0)))

;;; Remembering the fixtures which have been opened.
(defvar *opened-fixtures* (make-hash-table)
  "Maps fixture names to t to show that they have been opened.")
(defvar *opening-at-top* t
  "This tag will be dynamically set to nil when recurring over opening
fixtures; this should be used for output selection only.")

;;; Redirecting output from NST to elsewhere.
(defvar cl-user::*nst-default-report-stream* t
  "The default value for the stream to which NST reports output.")
