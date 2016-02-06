;;; File fixture.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
;;; Copyright (c) 2015, 2016 John Maraist
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
(in-package :nst)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-hashtable-fns fixture-bindings ()
    "from fixture name to the list of local variable names bound that fixture")
  (def-hashtable-fns fixture-letlist ()
    "from fixture name to the list of local variable names bound that fixture"))

(defstruct fixture-record
  "Information associated with a fixture definition"
  name function bound-names bindings-list documentation cache-flush)
(def-hashtable-fns fixture-record ()
    "from fixture name to its =fixture-record=")

(defmethod base-name ((fixture-record fixture-record))
  (fixture-record-name fixture-record))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun apply-fixtures (fixtures group-record test-records body-thunk)
  (cond
    (fixtures (apply-fixture (car fixtures) (cdr fixtures)
                             group-record test-records body-thunk))
    (t (funcall body-thunk #| group-record test-records |#))))

(defun apply-fixture (fixture next-fixtures
                      group-record test-records body-thunk)
  (cond
    ((symbolp fixture)
     (funcall (fixture-record-function (fixture-record fixture))
              next-fixtures group-record test-records body-thunk))
    ((functionp fixture) (funcall fixture next-fixtures
                                  group-record test-records body-thunk))
    (t (error "TODO anonymous fixtures not implemented"))))

;;; ------------------------------------------------------------------

(defun decode-fixture-syntax (fixture-name binding-specs
                              decls outer-decls
                              setup cleanup startup finish cache
                              specials use-fixtures special-variables)
  "Assemble syntactic elements of a fixture into the fixture function and
the list of names defined by the fixture."

  ;; Go through the SPECIAL and USE-FIXTURES lists to find additional
  ;; unbound variables.
  (loop for special in specials do
    (cond
      ((symbolp special) (push special special-variables))
      ((and (listp special) (eq :fixture (car special)))
       (loop for used-fixture-name in (cdr special) do
         (setf special-variables
               (append (fixture-bindings used-fixture-name)
                       special-variables))))))
  (loop for used-fixture-name in use-fixtures do
    (setf special-variables
          (append (fixture-bindings used-fixture-name) special-variables)))

  ;; First go through the list of the given bindings specifications to
  ;; find any with by-binding options declared.
  (multiple-value-bind (cache-names bindings ignore-names)
      (decode-caching-decls fixture-name binding-specs cache)

    ;; Pull the list of names from the bindings.
    (let* ((names (loop for binding in bindings collect (car binding)))

           ;; Start assembling the fixture function body.  At this
           ;; point we can make the binding to fixtures, with setup
           ;; and cleanup wrapping the call to other fixtures and the
           ;; core thunk.
           (core-exprs (cond

                         ;; If there are any bindings at all, then we
                         ;; abstract them here.
                         (names
                          `((let ,bindings
                              (declare (special ,@names) ,@decls
                                       ,@(when ignore-names
                                           `((ignore ,@ignore-names))))
                              ,@(when setup `(,setup))
                              (apply-fixtures fs group-record test-records
                                              thunk)
                              ,@(when cleanup `(,cleanup)))))

                         ;; No bindings at all.
                         (names
                          `(,@(when setup `(,setup))
                              (apply-fixtures fs group-record test-records
                                              thunk)
                              ,@(when cleanup `(,cleanup))))))

           ;; Body forms of the fixture function.
           (fixtures-function-body-forms
            `((block ,fixture-name
                ,@(when (or outer-decls (and decls (not names)))
                    `((declare ,@outer-decls
                               ,@(unless names decls))))
                ,@(when startup `(,startup))
                ,@core-exprs
                ,@(when finish `(,finish))))))

      (when special-variables
        (setf fixtures-function-body-forms
              `((locally (declare ,@(when special-variables
                                      `((special ,@special-variables))))
                  ,@fixtures-function-body-forms))))

      ;; Finally put together the fixture function, and also return
      ;; the list of defined names.
      (values `(lambda (fs group-record test-records thunk)
                 ,@fixtures-function-body-forms)
              names bindings cache-names))))

(defun decode-caching-decls (fixture-name binding-specs all-cache)
  "Process a list of binding specifications to extract both the bindings, and
the local variables needed for caching."
  (let ((ignore-names nil))

    ;; Process each binding in turn.
    (loop for spec in binding-specs

          ;; In a standard Common Lisp binding, the first item is the
          ;; local name (or the whole thing is just the local name).
          ;; In an NST fixture, the first item in the binding spec
          ;; could be a list of options.  So we first separate the
          ;; list of options from the list making up a standard Common
          ;; Lisp binding.
          for (options pre-binding)
            = (cond
                ((symbolp spec) (list nil (list spec)))
                ;; Some kind of error
                ((not (listp spec)) (list nil nil))
                ((symbolp (car spec)) (list nil spec))
                (t (list (car spec) (cdr spec))))
          for binding
            = (let ((param (car pre-binding)))
                ;; Fixture-bound names are allowed to be nil, to
                ;; simply give a side-effect.
                (unless param
                  (setf param (gensym))
                  (push param ignore-names))
                (list param
                      `(with-nst-control-handlers
                           ((e flag :cerror-label
                               ,(format nil "~@<Quit applying fixture ~s~:>"
                                  fixture-name)
                               :with-retry
                               ,(format nil "Calculate ~s for fixture ~s again."
                                  param fixture-name)
                               :handler-return-to ,fixture-name
                               :group group-record :tests test-records
                               :fail-test-msg "Error in fixture application"
                               :log-location ("in fixture")))
                         ,(cadr pre-binding))))

          ;; If the options specify that we need to cache this binding,
          ;; then create some additional local variables for it, and
          ;; wrap the binding's form to do the caching.
          for cache-name = (gensym)
          for cache-full-name = (gensym)
          if (destructuring-bind (&key (cache nil cache-supp-p)) options
               (if cache-supp-p cache all-cache))
            append (list cache-name cache-full-name) into cache-names
            and do
              (setf (cadr binding)
                    `(cond
                       (,cache-full-name ,cache-name)
                       (t (setf ,cache-name ,(cadr binding)
                                ,cache-full-name t)
                          ,cache-name)))
          end

          ;; Collect the binding in the list of all bindings.
          collect binding into bindings

          ;; We're finished and can return the list of bindings, and
          ;; the list of cache helper variables to which the bindings
          ;; refer.
          finally (return-from decode-caching-decls
                    (values cache-names bindings ignore-names)))))

;;; ------------------------------------------------------------------
;;; The big macro

#+allegro (excl::define-simple-parser def-fixtures second :nst-fixture-set)
(defmacro def-fixtures (name (&key (uses nil uses-supp-p)
                                   (assumes nil assumes-supp-p)
                                   special outer inner documentation cache
                                   setup cleanup startup finish export-names
                                   (export-bound-names
                                    nil export-bound-names-supp-p)
                                   (export-fixture-name
                                    nil export-fixture-name-supp-p))
                        &body bindings)
  "Fixtures are data structures and values which may be referred to by name
during testing.  NST provides the ability to use fixtures across multiple tests
and test groups, and to inject fixtures into the runtime namespace for
debugging.  A set of fixtures is defined using the =def-fixtures= macro:
#+begin_example
 \(def-fixtures fixture-name ( [ :outer FORM ]
                              [ :inner FORM ]
                              [ :setup FORM ]
                              [ :cleanup FORM ]
                              [ :startup FORM ]
                              [ :finish FORM ]
                              [ :documentation STRING ]
                              [ :cache FLAG ]
                              [ :export-names FLAG ]
                              [ :export-fixture-name FLAG ]
                              [ :export-bound-names FLAG ] )
   \( [ ( [ :cache FLAG ] ) ] NAME [ FORM ] )
   ...
   \( [ ( [ :cache FLAG ] ) ] NAME [ FORM ] ) )
#+end_example
- fixture-name :: The name to be associated with this set of fixtures.
- inner :: List of declarations to be made inside the let-binding of
     names of any use of this fixture.  Do not include the \"declare\"
     keyword here; NST adds these declarations to others, including a
     special declaration of all bound names.
- outer :: List of declarations to be made outside the let-binding of
     names of any use of this fixture.
- documentation :: A documentation string for the fixture set.
- special :: Specifies a list of names which should be declared
     =special= in the scope within which this set's fixtures are
     evaluated.  The individual names are taken to be single variable
     names.  Each =(:fixture NAME)= specifies all of the names of the given
     fixture set.  This declaration is generally optional under most
     platforms, but can help supress spurious warnings.  Note that
     multiple =NAMEs= may be listed, and these lists and the bare
     names may be intermixed.  If only one name or fixture is
     specified, it need not be placed in a list
- export-fixture-name :: When non-nil, the fixture name will be added
     to the list of symbols exported by the current package.
- export-bound-names :: When non-nil, the names bound by this fixture
     will be added to the list of symbols exported by the current
     package.
- export-names :: When non-nil, sets the default value to t for the
     two options above.
- cache :: If specified with the group options, when non-nil, the
     fixture values are cached at their first use, and re-applied at
     subsequent fixture application rather than being recalculated.

When a fixture is attached to a test or test group, each =NAME=
defined in that fixture becomes available in the body of that test or
group as if =let*= bound to the corresponding =FORM=.  A fixture in
one set may refer back to other fixtures in the same set (again as
=let*=) but forward references are not allowed.

The four arguments =startup=, =finish=, =setup= and =cleanup= specify
forms which are run everytime the fixture is applied to a group or
test.  The =startup= (respectively =finish=) form is run before
fixtures are bound (after their bindings are released).  These forms
are useful, for example, to initialize a database connection from
which the fixture values are drawn.  The =setup= form is run after
inclusion of names from fixture sets, but before any tests from the
group.  The =cleanup= form is normally run after the test completes,
but while the fixtures are still in scope.  Normally, the =cleanup=
form will not be run if the =setup= form raises an error, and the
=finish= form will not be run if the =startup= form raises an error;
although the user is able to select (perhaps unwisely) a restart which
disregards the error.

The names of a fixture and the names it binds can be exported from the
package where the fixture is defined using the =export-bound-names=
and =export-fixture-name= arguments.  The default value of both is the
value of =export-names=, whose default value is =nil=.

The =cache= option, if non-nil, directs NST to evaluate a fixture's
form one single time, and re-use the resulting value on subsequent
applications of the fixture.  Note that if this value is mutated by
the test cases, test behavior may become unpredictable!  However this
option can considerably improve performance when constant-valued
fixtures are applied repeatedly.  Caching may be set on or off (the
default is off) for the entire fixture set, and the setting may vary
for individual fixtures.

Examples of fixture definitions:
#+begin_example
\(def-fixtures f1 ()
  \(c 3)
  \(d 'asdfg))
\(def-fixtures f2 (:special ((:fixture f1)))
  \(d 4)
  \(e 'asdfg)
  \(f c))
\(def-fixtures f3 ()
  \((:cache t)   g (ackermann 1 2))
  \((:cache nil) h (factorial 5)))
#+end_example

To cause a side-effect among the evaluation of a fixture's name
definitions, =nil= can be provided as a fixture name.  In uses of the
fixture, NST will replace =nil= with a non-interned symbol; in
documentation such as form =:whatis=, any =nil=s are omitted."

  ;; Discourage deprecated forms
  (when uses-supp-p
    (warn 'nst-soft-keyarg-deprecation
          :old-name :uses :replacement :special))
  (when assumes-supp-p
    (warn 'nst-soft-keyarg-deprecation
          :old-name :assumes :replacement :special))

  ;; Propagate catchall export switch
  (when export-names
    (unless export-bound-names-supp-p
      (setf export-bound-names t))
    (unless export-fixture-name-supp-p
      (setf export-fixture-name t)))

  ;; Decode the syntax for the fixture function and the list of bound
  ;; names.
  (multiple-value-bind (fixture-function fixture-bindings let-list cache-names)
      (decode-fixture-syntax name bindings inner outer setup
                             cleanup startup finish cache
                             special uses assumes)
    `(progn

       ,@(loop for cname in cache-names
             collect `(defvar ,cname nil
                        ,(format nil "Cache variable for NST fixture ~s" name)))

       ;; Things we'll need at compile-time: first the bound names.
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (fixture-bindings ',name) ',fixture-bindings
               (fixture-letlist ',name) ',let-list)

         ;; Export bound names
         ,@(when export-bound-names
             (loop for id in fixture-bindings
                 collect `(export ',id
                                  ,(intern (package-name (symbol-package id))
                                           (find-package :keyword)))))

         ;; Export the fixture name
         ,@(when export-fixture-name
             `((export ',name ,(intern (package-name (symbol-package name))
                                       (find-package :keyword))))))

       ;; Save the other compiled artifacts,
       (let ((record
              (make-fixture-record :name ',name
                                   :function #',fixture-function
                                   :bound-names ',fixture-bindings
                                   :bindings-list ',let-list
                                   :documentation ,documentation
                                   :cache-flush
                                   #'(lambda ()
                                       ,@(cond
                                           (cache-names
                                            `((setf ,@(loop for n in cache-names
                                                          append `(,n nil)))))
                                           (t '(nil)))))))
         (setf (fixture-record ',name) record)
         (record-name-use record))

       ;; Return the name of the fixture.
       ',name)))


(defmacro with-fixtures ((&rest fixtures) &body forms)
  "The =with-fixtures= macro faciliates debugging and other non-NST uses of
fixtures sets:
#+begin_example
\(with-fixtures (FIXTURE FIXTURE ... FIXTURE)
  FORM
  FORM
  ...
  FORM)
#+end_example
This macro evaluates the forms in a namespace expanded with the bindings
provided by the fixtures.
"
  (loop for fixture in fixtures
        for fixture-record = (fixture-record fixture)
        append (fixture-bindings fixture) into fixture-bindings
        collect (fixture-record-function fixture-record) into fixture-functions
        finally
     (return-from with-fixtures
       (cond
         ((null fixture-functions) `(progn ,@forms))
         (t `(apply-fixtures fixture-functions nil nil
                             #'(lambda ()
                                 (declare (special ,@fixture-bindings))
                                 ,@forms)))))))


(defun open-fixture (name &optional (in-package *package*))
  "Push the bindings from a fixture into a package's namespace."
  (unless (packagep in-package)
    (setf in-package (find-package in-package)))
  (let ((bindings (fixture-letlist name)))
    (loop for (name val) in bindings do
      (setf (symbol-value (intern (symbol-name name) in-package)) (eval val))))
  name)


(defun flush-fixture-cache (name)
  (let ((fixture-record (fixture-record name)))
    (when fixture-record
      (let ((flusher (fixture-record-cache-flush fixture-record)))
        (when flusher
          (funcall flusher))))))
