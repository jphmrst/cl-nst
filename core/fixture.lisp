;;; File fixture.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
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

(defvar *fixture-functions* (make-hash-table :test 'eq)
  "Map from a fixture name to the associated fixture function.")

(defun fixture-function (name)
  "Return the fixture function associated with the given name."
  (gethash name *fixture-functions*))

(defun (setf fixture-function) (new-fixture-function name)
  "Associate a function with the given fixture name.  This is a low-level
setting, and does not change e.g. *fixture-function-bindings*."
  (setf (gethash name *fixture-functions*) new-fixture-function))

;;; ------------------------------------------------------------------

(defvar *fixture-bindings* (make-hash-table :test 'eq)
  "Map from fixture name to the list of local variable names bound that
fixture.")

(defun fixture-bindings (name)
  "Return the names bound by the fixture associated with the given name."
  (gethash name *fixture-bindings*))

(defun (setf fixture-bindings) (new-fixture-bindings name)
  "Associate a list of names as bound by fixtures with the given name."
  (setf (gethash name *fixture-bindings*) new-fixture-bindings))

;;; ------------------------------------------------------------------

(defun decode-fixture-syntax (binding-specs &optional decls outer-decls
                                              setup cleanup startup finish
                                              cache)
  "Assemble syntactic elements of a fixture into the fixture function and
the list of names defined by the fixture."

  ;; First go through the list of the given bindings specifications to
  ;; find any with by-binding options declared.
  (multiple-value-bind (cache-defs bindings)
      (decode-caching-decls binding-specs cache)

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
                              (declare (special ,@names) ,@decls)
                              ,@(when setup `(,setup))
                              (cond
                                (fs (funcall (car fs) (cdr fs) thunk))
                                (t (funcall thunk)))
                              ,@(when cleanup `(,cleanup)))))

                         ;; No bindings at all.
                         (names
                          `(,@(when setup `(,setup))
                            (cond
                              (fs (funcall (car fs) (cdr fs) thunk))
                              (t (funcall thunk)))
                            ,@(when cleanup `(,cleanup)))))))

      ;; If we're caching any results at all, then wrap the core
      ;; expressions inside bindings for variables to manage that
      ;; cache.
      (when cache-defs
        (setf core-exprs `((let ,cache-defs ,@core-exprs))))

      ;; Finally put together the fixture function, and also return
      ;; the list of defined names.
      (values (eval `(lambda (fs thunk)
                       ,@(when (or outer-decls (and decls (not names)))
                           `((declare ,@outer-decls
                                      ,@(unless names decls))))
                       ,@(when startup `(,startup))
                       ,@core-exprs
                       ,@(when finish `(,finish))))
              names))))

(defun decode-caching-decls (binding-specs all-cache)
  "Process a list of binding specifications to extract both the bindings, and
the local variables needed for caching."

  ;; Process each binding in turn.
  (loop for spec in binding-specs

        ;; In a standard Common Lisp binding, the first item is the
        ;; local name (or the whole thing is just the local name).  In
        ;; an NST fixture, the first item in the binding spec could be
        ;; a list of options.  So we first separate the list of
        ;; options from the list making up a standard Common Lisp
        ;; binding.
        for (options binding) = (cond
                                  ((symbolp spec) (list nil (list spec)))
                                  ;; Some kind of error
                                  ((not (listp spec)) (list nil nil))
                                  ((symbolp (car spec)) (list nil spec))
                                  (t (list (car spec) (cdr spec))))

        ;; If the options specify that we need to cache this binding,
        ;; then create some additional local variables for it, and
        ;; wrap the binding's form to do the caching.
        for cache-name = (gensym)
        for cache-full-name = (gensym)
        if (destructuring-bind (&key (cache nil cache-supp-p)) options
             (if cache-supp-p cache all-cache))
          append `((,cache-name nil) (,cache-full-name nil)) into cache-defs
          and do (setf (cadr binding)
                       `(cond
                          (,cache-full-name ,cache-name)
                          (t (let ((,cache-name ,(cadr binding)))
                               (setf ,cache-full-name t)
                               ,cache-name))))
        end

        ;; Collect the binding in the list of all bindings.
        collect binding into bindings

        ;; We're finished and can return the list of bindings, and the
        ;; list of cache helper variables to which the bindings refer.
        finally (return-from decode-caching-decls
                  (values cache-defs bindings))))

;;; ------------------------------------------------------------------
;;; The big macro

#+allegro (excl::define-simple-parser def-fixtures second :nst-fixture-set)
(defmacro def-fixtures (name (&key (uses nil uses-supp-p)
                                   (assumes nil assumes-supp-p)
                                   special outer inner documentation cache
                                   setup cleanup startup finish export-names
                                   export-bound-names export-fixture-name)
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
- documentation :: TODO A documentation string for the fixture set.
- special :: TODO Specifies a list of names which should be declared
     =special= in the scope within which this set's fixtures are
     evaluated.  The individual names are taken to be single variable
     names.  Each =(:fixture NAME)= specifies all of the names of the given
     fixture set.  This declaration is generally optional under most
     platforms, but can help supress spurious warnings.  Note that
     multiple =NAMEs= may be listed, and these lists and the bare
     names may be intermixed.  If only one name or fixture is
     specified, it need not be placed in a list
- export-fixture-name :: TODO When non-nil, the fixture name will be added
     to the list of symbols exported by the current package.
- export-bound-names :: TODO When non-nil, the names bound by this fixture
     will be added to the list of symbols exported by the current
     package.
- export-names :: TODO When non-nil, sets the default value to t for the
     two options above.
- cache :: TODO If specified with the group options, when non-nil, the
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
\(def-fixtures f2 (:special (:fixture f1))
  \(d 4)
  \(e 'asdfg)
  \(f c))
\(def-fixtures f3 ()
  \((:cache t)   g (ackermann 1 2))
  \((:cache nil) h (factorial 5)))
#+end_example

TODO To cause a side-effect among the evaluation of a fixture's name
definitions, =nil= can be provided as a fixture name.  In uses of the
fixture, NST will replace =nil= with a non-interned symbol; in
documentation such as form =:whatis=, any =nil=s are omitted."

  ;; Unimplemented or now-disregarded options.
  (declare (ignore assumes special export-names export-bound-names
                   export-fixture-name uses documentation))

  ;; Discourage deprecated forms
  (when uses-supp-p
    (warn 'nst-soft-keyarg-deprecation
      :old-name :uses :replacement :special))
  (when assumes-supp-p
    (warn 'nst-soft-keyarg-deprecation
          :old-name :assumes :replacement :special))

  ;; Decode the syntax for the fixture function and the list of bound
  ;; names.
  (multiple-value-bind (fixture-function fixture-bindings)
      (decode-fixture-syntax bindings inner outer setup cleanup startup finish
                             cache)

    ;; Save the compiled artifacts,
    (setf (fixture-function name) fixture-function)
    (setf (fixture-bindings name) fixture-bindings)

    ;; Return the name of the fixture.
    name))

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
        append (fixture-bindings fixture) into fixture-bindings
        collect (fixture-function fixture) into fixture-functions
        finally (return-from with-fixtures
                  (cond
                    ((null fixture-functions) `(progn ,@forms))
                    (t `(funcall ,(car fixture-functions)
                                 ,(cdr fixture-functions)
                                 #'(lambda ()
                                     (declare (special ,@fixture-bindings))
                                     ,@forms)))))))
