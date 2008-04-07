



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
