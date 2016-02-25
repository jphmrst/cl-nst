- [Fixtures](#fixtures)
- [Test groups](#test-groups)
- [Testing values](#testing-values)
- [Testing processes](#testing-processes)
- [Testing invariants against sampled data](#testing-invariants-against-sampled-data)
- [Defining test criteria](#defining-test-criteria)
- [The runtime system](#the-runtime-system)
- [Integration with ASDF](#integration-with-asdf)
- [Inheritance-based test methods](#inheritance-based-test-methods)
- [Deprecated forms](#deprecated-forms)

This document is the manual and users' guide to the 4.1.\(x\) series of
the NST test framework, last updated for 4.1.0.  NST is a unit test
system for Common Lisp which provides support for test fixture data,
stateful setup and cleanup of tests, grouping of tests, and (we
think!) a useful runtime interface.  Suggestions and comments are
welcome.  The files in the NST distribution's `self-test`
directory, especially `self-test/core/builtin-checks.lisp`,
holds the NST tests for NST and contain many examples (some of which
we have adapted for this manual).  Known bugs and infelicities,
platform-specific release notes, and other technical materials are
available via the link on NST's CLiki page, `cliki.net/NST`.

**Contributors.** The primary author of both NST and this manual is John
Maraist (Smart Information Flow Technologies, 211 North First Street,
Suite 300, Minneapolis, MN 55401; `jmaraist` at `sift.info`).  Robert
P. Goldman provided guidance, comments and suggestions through the
development.  Other contributors include Michael J.S. Pelican,
Steven A. Harp, Michael Atighetchi and Patrick Stein.

# Fixtures<a id="orgheadline1"></a>

Fixtures are data structures and values which may be referred to by name
during testing.  NST provides the ability to use fixtures across multiple tests
and test groups, and to inject fixtures into the runtime namespace for
debugging.  A set of fixtures is defined using the `def-fixtures` macro:

    (def-fixtures fixture-name ( [ :outer FORM ]
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
      ( [ ( [ :cache FLAG ] ) ] NAME [ FORM ] )
      ...
      ( [ ( [ :cache FLAG ] ) ] NAME [ FORM ] ) )

-   **fixture-name:** The name to be associated with this set of fixtures.
-   **inner:** List of declarations to be made inside the let-binding of
    names of any use of this fixture.  Do not include the "declare"
    keyword here; NST adds these declarations to others, including a
    special declaration of all bound names.
-   **outer:** List of declarations to be made outside the let-binding of
    names of any use of this fixture.
-   **documentation:** A documentation string for the fixture set.
-   **special:** Specifies a list of names which should be declared
    `special` in the scope within which this set's fixtures are
    evaluated.  The individual names are taken to be single variable
    names.  Each `(:fixture NAME)` specifies all of the names of the given
    fixture set.  This declaration is generally optional under most
    platforms, but can help supress spurious warnings.  Note that
    multiple `NAMEs` may be listed, and these lists and the bare
    names may be intermixed.  If only one name or fixture is
    specified, it need not be placed in a list
-   **export-fixture-name:** When non-nil, the fixture name will be added
    to the list of symbols exported by the current package.
-   **export-bound-names:** When non-nil, the names bound by this fixture
    will be added to the list of symbols exported by the current
    package.
-   **export-names:** When non-nil, sets the default value to t for the
    two options above.
-   **cache:** If specified with the group options, when non-nil, the
    fixture values are cached at their first use, and re-applied at
    subsequent fixture application rather than being recalculated.

When a fixture is attached to a test or test group, each `NAME`
defined in that fixture becomes available in the body of that test or
group as if `let*` bound to the corresponding `FORM`.  A fixture in
one set may refer back to other fixtures in the same set (again as
`let*`) but forward references are not allowed.

The four arguments `startup`, `finish`, `setup` and `cleanup` specify
forms which are run everytime the fixture is applied to a group or
test.  The `startup` (respectively `finish`) form is run before
fixtures are bound (after their bindings are released).  These forms
are useful, for example, to initialize a database connection from
which the fixture values are drawn.  The `setup` form is run after
inclusion of names from fixture sets, but before any tests from the
group.  The `cleanup` form is normally run after the test completes,
but while the fixtures are still in scope.  Normally, the `cleanup`
form will not be run if the `setup` form raises an error, and the
`finish` form will not be run if the `startup` form raises an error;
although the user is able to select (perhaps unwisely) a restart which
disregards the error.

The names of a fixture and the names it binds can be exported from the
package where the fixture is defined using the `export-bound-names`
and `export-fixture-name` arguments.  The default value of both is the
value of `export-names`, whose default value is `nil`.

The `cache` option, if non-nil, directs NST to evaluate a fixture's
form one single time, and re-use the resulting value on subsequent
applications of the fixture.  Note that if this value is mutated by
the test cases, test behavior may become unpredictable!  However this
option can considerably improve performance when constant-valued
fixtures are applied repeatedly.  Caching may be set on or off (the
default is off) for the entire fixture set, and the setting may vary
for individual fixtures.

Examples of fixture definitions:

    (def-fixtures f1 ()
      (c 3)
      (d 'asdfg))
    (def-fixtures f2 (:special ((:fixture f1)))
      (d 4)
      (e 'asdfg)
      (f c))
    (def-fixtures f3 ()
      ((:cache t)   g (ackermann 1 2))
      ((:cache nil) h (factorial 5)))

To cause a side-effect among the evaluation of a fixture's name
definitions, `nil` can be provided as a fixture name.  In uses of the
fixture, NST will replace `nil` with a non-interned symbol; in
documentation such as form `:whatis`, any =nil=s are omitted.

The `with-fixtures` macro faciliates debugging and other non-NST uses of
fixtures sets:

    (with-fixtures (FIXTURE FIXTURE ... FIXTURE)
      FORM
      FORM
      ...
      FORM)

This macro evaluates the forms in a namespace expanded with the bindings
provided by the fixtures.

# Test groups<a id="orgheadline2"></a>

The `def-test-group` form defines a group of the
given name, providing one instantiation of the bindings of the given fixtures
to each test.  Groups can be associated with fixture sets, stateful
initialization, and stateful cleanup.

    (def-test-group NAME (FIXTURE FIXTURE ...)
      (:aspirational FLAG)
      (:setup FORM FORM ... FORM)
      (:cleanup FORM FORM ... FORM)
      (:startup FORM FORM ... FORM)
      (:finish FORM FORM ... FORM)
      (:each-setup FORM FORM ... FORM)
      (:each-cleanup FORM FORM ... FORM)
      (:include-groups GROUP GROUP ... GROUP)
      (:documentation STRING)
      TEST
      ...
      TEST)

Arguments:

-   **group-name:** Name of the test group being defined
-   **given-fixtures:** List of the names of fixtures and anonymous fixtures to be used with the tests in this group.
-   **aspirational:** An aspirational test is one which verifies some part of an API or code contract which may not yet be implemented.  When a group is marked aspirational, all tests within the group are taken to be aspirational as well.
    At this point, there is no particular processing for aspirational tests and groups, but we hope to implement it at some point in the future.
-   **forms:** Zero or more test forms, given by `def-check`.
-   **setup:** These forms are run once, before any of the individual tests, but after the fixture names are bound.
-   **cleanup:** These forms are run once, after all of the individual tests, but while the fixture names are still bound.
-   **startup:** These forms are run once, before any of the individual tests and before the fixture names are bound.
-   **finish:** These forms are run once, after all of the individual tests, and after the scope of the bindings to fixture names.
-   **each-setup:** These forms are run before each individual test.
-   **each-cleanup:** These forms are run after each individual test.
-   **include-group:** The test groups named in this form will be run (respectively reported) anytime this group is run (reported).
-   **documentation:** Docstring for the class.

# Testing values<a id="orgheadline43"></a>

Individual unit tests are encoded with the `def-test` form:

    (def-test NAME ( [ :group GROUP-NAME ]
                      [ :setup FORM ]
                      [ :cleanup FORM ]
                      [ :startup FORM ]
                      [ :finish FORM ]
                      [ :fixtures (FIXTURE FIXTURE ... FIXTURE) ]
                      [ :aspirational FLAG ]
                      [ :documentation STRING ] )
        criterion
     FORM ... FORM)
    
    (def-test NAME criterion &body (:seq FORM))

The `SETUP`, `CLEANUP`, `STARTUP`, `FINISH` and `FIXTURES` are just as for
fixtures and test groups, but apply only to the one test.  The `CRITERION`
is a list or symbol specifying the properties which should hold for
the =FORM=s.

When a test is not enclosed within a group body, a group name must be
provided by the `GROUP` option.  When a test is enclosed within
a group body, the `GROUP` option is not required, but if
provided it must agree with the group name.

When there are no `SETUP`, `CLEANUP`, `STARTUP`, `FINISH` or `FIXTURES`
arguments, the `NAME` may be given without parentheses.  Likewise, any
criterion consisting of a single symbol, e.g. `(:pass)`, may be abbreviated
as just the symbol without the parentheses, e.g. `:pass`.

The `:documentation` form provides a documentation string in
the standard Lisp sense.  Since documentation strings are stored
against names, and since the same name can be used for several tests
(so long as they are all in different packages), documentation strings
on tests may not be particularly useful.

An `aspirational` test is one which verifies some part of an API or code
contract which may not yet be implemented.  When a group is marked
aspirational, all tests within the group are taken to be aspirational as well.
At this point, there is no particular processing for aspirational tests and
groups, but we hope to implement it at some point in the future.

The `def-check` form is a deprecated synonym for `def-test`.

## Basic criteria<a id="orgheadline15"></a>

### Criterion `true`<a id="orgheadline3"></a>

The `:true` criterion expects one form, which is evaluated at testing time; the criterion requires the result to be non-nil.

### Criterion `eq`<a id="orgheadline4"></a>

The `:eq` criterion checks a form using `eq`.  The criterion argument and the form under test are both evaluated at testing time.

Usage:

    (:eq TARGET)

Example:

    (def-test eq1 (:eq 'b) (cadr '(a b c)))

### Criterion `symbol`<a id="orgheadline5"></a>

The `:symbol` criterion checks that its form under test evaluates to a symbol which is `eq` to the symbol name given as the criterion argument.

Usage:

    (:symbol NAME)

Example:

    (def-test sym1  (:symbol a) (car '(a b c)))

Example of a test which fails:

    (def-test sym1x (:symbol a) (cadr '(a b c)))

### Criterion `eql`<a id="orgheadline6"></a>

The `:eql` criterion checks a form using `eql`.  The criterion argument and the form under test are both evaluated at testing time.

Usage:

    (:eql TARGET)

Example:

    (def-test eql1 (:eql 2) (cadr '(1 2 3)))

### Criterion `equal`<a id="orgheadline7"></a>

The `:equal` criterion checks a form using `eql`.  The criterion argument and the form under test are both evaluated at testing time.

### Criterion `equalp`<a id="orgheadline8"></a>

The `:equalp` criterion checks a form using `equalp`.  The criterion argument and the form under test are both evaluated at testing time.

Usage:

    (:equalp TARGET)

### Criterion `forms-eq`<a id="orgheadline9"></a>

The `:forms-eq` criterion compares its two forms under test using `eq`.  The forms are both evaluated at testing time.

Usage:

    (:forms-eq)

Example:

    (def-test eqforms1 :forms-eq (cadr '(a b c)) (caddr '(a c b)))

### Criterion `forms-eql`<a id="orgheadline10"></a>

The `:forms-eql` criterion compares its two forms under test using `eql`.  The two forms under test are both evaluated at testing time.

Usage:

    (:forms-eql)

Example:

    (def-test eqlforms1 :forms-eql (cadr '(a 3 c)) (caddr '(a c 3)))

### Criterion `forms-equal`<a id="orgheadline11"></a>

The `:forms-equal` criterion compares its two forms under test using `equal`.  The forms are both evaluated at testing time.

Usage:

    (:forms-equal)

### Criterion `predicate`<a id="orgheadline12"></a>

The `:predicate` criterion applies a predicate to the result of evaluating its form under test.  The criterion argument is a symbol (unquoted) or a lambda expression; at testing time, the forms under test are evaluated and passed to the denoted function.  The criterion expects that the result of the function is non-nil.

Usage:

    (:predicate PRED)

Example:

    (def-test pred1 (:predicate numberp) 3)

Example of a test which fails:

    (def-test pred2 (:predicate eql) (+ 1 2) 3)

### Criterion `err`<a id="orgheadline13"></a>

The `:err` criterion evaluates the form under test, expecting the evaluation to raise some condition.  If the `class` argument is supplied, the criterion expects the raised condition to be a subclass.  Note that the name of the type should *not* be quoted; it is not evaluated.

Usage:

    (:err &KEY (TYPE CLASS))

Examples:

    (def-test err1 (:err :type error) (error "this should be caught"))
    (def-test err2 (:err) (error "this should be caught"))

### Criterion `perf`<a id="orgheadline14"></a>

The `:perf` criterion evaluates the forms under test at testing time, checking that the evaluation completes within the given time limit.

Usage:

    (:perf &KEY (MS MILLISECS) (SEC SECONDS) (MIN MINUTES))

Example:

    (def-test perf1 (:perf :min 2) (ack 3 5))

## Compound criteria<a id="orgheadline25"></a>

### Criterion `not`<a id="orgheadline16"></a>

The `:not` criterion passes when testing according to `subcriterion` fails (but does not throw an error).

Usage:

    (:not SUBCRITERION)

Example:

    (def-test not1 (:not (:symbol b)) 'a)

### Criterion `all`<a id="orgheadline17"></a>

The `:all` criterion brings several other criteria under one check, and verifies that they all pass.

Usage:

    (:all SUBCRITERION ... SUBCRITERION)

Example:

    (def-check not1 ()
        (:all (:predicate even-p)
              (:predicate prime-p))
      2)

### Criterion `any`<a id="orgheadline18"></a>

The `:any` criterion passes when any of the subordinate criteria pass.

Usage:

    (:any SUBCRITERION ... SUBCRITERION)

Example:

    (def-check not1 ()
        (:any (:predicate even-p)
              (:predicate prime-p))
      5)

### Criterion `apply`<a id="orgheadline19"></a>

The `:apply` criterion first evaluates the forms under test, applying `function` to them.  The overall criterion passes or fails exactly when the subordinate `criterion` with the application's multiple result values.

Usage:

    (:apply FUNCTION CRITERION)

Example:

    (def-test applycheck (:apply cadr (:eql 10)) '(0 10 20))

### Criterion `check-err`<a id="orgheadline20"></a>

The `:check-err` criterion is like `:err`, but proceeds according to the subordinate criterion rather than simply evaluating the input forms.

Usage:

    (:check-err CRITERION)

Example:

    (def-test check-err1
        (:check-err :forms-eq)
      'asdfgh (error "this should be caught"))

### Criterion `progn`<a id="orgheadline21"></a>

The `:progn` criterion first evaluates the =FORM=s in order, and then proceeds with evaluation of the forms under test according to the subordinate criterion.

Usage:

    (:progn FORM ... FORM SUBCRITERION)

Example:

    (def-test form1 (:progn (setf zz 3) (:eql 3)) zz)

### Criterion `proj`<a id="orgheadline22"></a>

The `:proj` criterion rearranges the forms under test by selecting a new list according to the index numbers into the old list.  Checking of the reorganized forms continues according to the subordinate criterion.

Usage:

    (:proj INDICES CRITERION)

Example:

    (def-test proj-1
        (:proj (0 2) :forms-eq)
      'a 3 (car '(a b)))

### Criterion `with-common-criterion`<a id="orgheadline23"></a>

The `:with-common-criterion` criterion applies one criterion to several data forms.

Usage:

    (:with-common-criterion [ CRITERION | (CRITERION ARG ... ARG) ]
                            (FORM ...  FORM) ... (FORM ...  FORM) )

### Criterion `applying-common-criterion`<a id="orgheadline24"></a>

The `:applying-common-criterion` criterion applies one criterion to several pairs of criterion arguments and data forms.

Usage:

    (:applying-common-criterion [ CRITERION | (CRITERION ARG ... ARG) ]
                                ( ((ARG ... ARG) (FORM ... FORM))
                                  ...
                                  ((ARG ... ARG) (FORM ... FORM)) )
                                ...
                                ( ((ARG ... ARG) (FORM ... FORM))
                                  ...
                                  ((ARG ... ARG) (FORM ... FORM)) ) )

## Criteria for multiple values<a id="orgheadline29"></a>

### Criterion `values`<a id="orgheadline26"></a>

The `:values` criterion checks each of the forms under test according to the respective subordinate criterion.

Usage:

    (:values SUBCRITERION ... SUBCRITERION)

### Criterion `value-list`<a id="orgheadline27"></a>

The `:value-list` criterion converts multiple values into a single list value.

Usage:

    (:value-list FURTHER)

### Criterion `drop-values`<a id="orgheadline28"></a>

The `:drop-values` criterion checks the primary value according to the subordinate criterion, ignoring any additional returned values from the evaluation of the form under test.

Usage:

    (:drop-values CRITERION)

## Criteria for lists<a id="orgheadline34"></a>

### Criterion `seq`<a id="orgheadline30"></a>

The `:seq` criterion evaluates its input form, checks each of its elements according to the respective subordinate criterion, and passes when all of them pass.

Usage:

    (:seq SUBCRITERION ... SUBCRITERION)

Example:

    (def-check seqcheck
        (:seq (:predicate symbolp) (:eql 1) (:symbol d))
      '(a 1 d))

### Criterion `each`<a id="orgheadline31"></a>

The `:each` criterion evaluates the form under test, expecting to find a list as a result.  Expects that each argument of the list according to the subordinate `criterion`, and passes when all of these checks pass.

Usage:

    (:each CRITERION)

Example:

    (def-test each1 (:each (:symbol a)) '(a a a a a))

### Criterion `permute`<a id="orgheadline32"></a>

The `:permute` criterion evaluates the form under test, expecting to find a list as a result.  The criterion expects to find that some permutation of this list will satisfy the subordinate criterion.

Usage:

    (:permute CRITERION)

Example:

    (def-test permute1 (:permute (:each (:eq 'a))) '(a a))
    (def-check permute2
        (:permute (:seq (:symbol b)
                        (:predicate symbolp)
                        (:predicate numberp)))
      '(1 a b))

### Criterion `alist`<a id="orgheadline33"></a>

The `:alist` criterion evaluates the form under test, expecting to find an association list as a result.  Using the two given function specs to test the keys (during retrieval, via `assoc`) and the values, the criterion enforces that the association lists contains exactly equivalent keys, mapping to respective equivalent values.  Implemented using `:alist*` plus a check of the list `length`, which could be incorrect if the criterion lists duplicate keys.

Usage:

    (:alist KEY-TEST-FN VALUE-TEST-FN (KEY VALUE) ... (KEY VALUE))

## Criteria for vectors<a id="orgheadline36"></a>

### Criterion `across`<a id="orgheadline35"></a>

The `:across` criterion is like `:seq`, but for a vector instead of a list.

Usage:

    (:across SUBCRITERION ... SUBCRITERION)

Example:

    (def-check across1
        (:across (:predicate symbolp) (:eql 1))
      (vector 'a 1))

## Criteria for classes<a id="orgheadline38"></a>

### Criterion `slots`<a id="orgheadline37"></a>

The `:slots` criterion evaluates its input form, and passes when the value at each given slot satisfies the corresponding subordinate constraint.

Usage:

    (:slots (SLOT-NAME SUBCRITERION) ... (SLOT-NAME SUBCRITERION))

Example:

    (defclass classcheck ()
      ((s1 :initarg :s1 :reader get-s1)
       (s2 :initarg :s2)
       (s3 :initarg :s3)))
     (def-test slot1
         (:slots (s1 (:eql 10))
                 (s2 (:symbol zz))
                 (s3 (:seq (:symbol q) (:symbol w)
                           (:symbol e) (:symbol r))))
       (make-instance 'classcheck
         :s1 10 :s2 'zz :s3 '(q w e r)))

## Programmatic and debugging criteria<a id="orgheadline42"></a>

### Criterion `pass`<a id="orgheadline39"></a>

The `:pass` is a trivial test which always passes.

Example:

    (def-test passing-test :pass 3 4 "sd")

### Criterion `warn`<a id="orgheadline40"></a>

The `:warn` criterion issues a warning.  The format string and arguments should be suitable for the Lisp `format` function.

Usage:

    (:warn FORMAT-STRING FORM ... FORM)

Example:

    (:warn "~{}d is not a perfect square" 5)

### Criterion `info`<a id="orgheadline41"></a>

The `:info` criterion adds an informational note to the check result.

Usage:

    (:info STRING SUBCRITERION)

Example:

    (def-test known-bug (:info "Known bug" (:eql 3)) 4)

# Testing processes<a id="orgheadline50"></a>

The test criteria of the previous section all examined the result of
evaluating the forms under test.  This section presents NST's criteria
for validating the *process* of a computation, specifying assertions
which should hold at the initial, intermediate and final points of the
process.

## Criterion `eval`<a id="orgheadline44"></a>

    (def-criterion ( [ :check-warnings FLAG ] [ :muffle-warnings FLAG ]
                     [ :attempt-continue FLAG ] [ :force-continue FLAG ] )
      FORM
      ...
      FORM)

The `:eval` criterion executes its forms, expecting calls to various assertion
functions to check intermediate states of an arbitrarily-long process.

-   **check-warnings:** If non-nil, will add warnings thrown when evaluating the forms under test as NST warnings.  The default is `t`.
-   **muffle-warnings:** If non-nil, will muffle warnings thrown when evaluating the forms under test, so that they are reported only as NST result warnings and if the `:check-warnings` flag is set.  The default is `t`.
-   **attempt-continue:** If non-nil, will continue evaluation after failed assertions, so long as the failure is not deemed `fatal`. The default is `t`.
-   **force-continue:** If non-nil, will continue evaluation after failed assertions even if the failure is not deemed `fatal`. The default is `nil`.

## Defining tests<a id="orgheadline45"></a>

The `def-eval-test` macro abbreviates a call to `def-test` with a single
`:eval` criterion.  Its arguments are just as for `def-test` and `:eval`.

    (def-eval-test (NAME [ :group GROUP-NAME ]
                         [ :setup FORM ]
                         [ :cleanup FORM ]
                         [ :startup FORM ]
                         [ :finish FORM ]
                         [ :fixtures (FIXTURE ... FIXTURE) ]
                         [ :documentation STRING ]
                         [ :check-warnings FLAG ]
                         [ :muffle-warnings FLAG ]
                         [ :attempt-continue FLAG ]
                         [ :force-continue FLAG ] )
      FORM
      ...
      FORM)
    
    (def-eval-test NAME
      FORM
      ...
      FORM)

## Asserting properties<a id="orgheadline46"></a>

The `assert-non-nil` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It succeeds whenever the `null` function returns `nil`.

The `assert-not-eq` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It compares the expected and tested values using `eq`, succeeds whenever that call returns `nil`.

The `assert-not-equalp` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It compares the expected and tested values using `equalp`, succeeds whenever that call returns `nil`.

The `assert-null` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It succeeds whenever the `null` function returns non-nil.

The `assert-criterion` macro asserts that an NST criterion should pass.

    (assert-criterion ( [ :msg-format FORMAT-STRING ]
                        [ :msg-args FORMAT-ARGUMENTS ]
                        [ :fatal FLAG [
                        [ :fail-on-warning FLAG ] )
      FORM
      ...
      FORM)

-   **msg-format:** Format string used to build the label of the restart point.
-   **msg-args:** Format arguments used to build the label of the restart point.
-   **fatal:** If non-nil, a failure of this assertion indicates that execution of
    the test forms should be aborted.
-   **fail-on-warning:** If non-nil, then an NST result which includes a warning
    indicates failure of this assertion.

The `assert-equal` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It compares the expected and tested values using `equal`, succeeds whenever that call returns non-nil.

The `assert-zero` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It succeeds whenever the `zerop` function returns non-nil.

The `assert-equalp` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It compares the expected and tested values using `equalp`, succeeds whenever that call returns non-nil.

The `assert-not-equal` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It compares the expected and tested values using `equal`, succeeds whenever that call returns `nil`.

The `assert-eq` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It compares the expected and tested values using `eq`, succeeds whenever that call returns non-nil.

The `assert-eql` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It compares the expected and tested values using `eql`, succeeds whenever that call returns non-nil.

The `assert-not-eql` function is a unary predicate for use within the forms evaluated for an `:eval` criterion.  It compares the expected and tested values using `eql`, succeeds whenever that call returns `nil`.

## Defining new assertion functions<a id="orgheadline47"></a>

Macro `macro def-unary-predicate-assert` creates an assertion function using
the result of a call to a unary predicate.  A non-nil result from the predicate
corresponds to a successful assertion.

    (def-unary-predicate-assert ASSERT-FN PREDICATE DEFAULT-MESSAGE
                                [ :message-defvar NAME ]
                                [ :pred-name NAME ]
                                [ :doc-state-flag BOOL ] )

-   **assert-fn:** The name of the assertion function being defined.
-   **predicate:** The predicate used to define the assertion function.  It should take a single argument.
-   **default-message:** Format string used by default for reporting failures of this assertion.  It should expect to be used in a call to `format` with one additional argument, the value being tested.
-   **message-defvar:** The name of a global variable into which the default message will be stored.  If this argument is omitted, the result of a call to `gensym` is used.
-   **pred-name:** This argument is used only for documenting the underlying predicate in the assertion function's docstring.  By default, it is the same as the predicate.

Macro `def-binary-predicate-assert` uses a binary predicate as the basis for
an assertion function just as `def-unary-predicate-assert` uses a unary
predicate.  This macro's arguments are just as for
`def-unary-predicate-assert`.

Macro `def-unary-negated-predicate-assert` uses the negated result of a
unary predicate as the basis of an assertion function.  This macro's arguments
are just as for `def-unary-predicate-assert`.

Macro `def-binary-negated-predicate-assert` uses the negated result of a
binary predicate as the basis for an assertion function just as
`def-unary-negated-predicate-assert` uses the negated result of a unary
predicate.  This macro's arguments are just as for
`def-unary-predicate-assert`.

## A simpler process checker<a id="orgheadline49"></a>

### Criterion `process`<a id="orgheadline48"></a>

The `:process` criterion allows simple interleaving of Lisp function
calls and NST checks, to allow checking of intermediate states of an
arbitrarily-long process.

This criterion takes as its body a list of forms.  The first element of each
form should be a symbol:

-   **`:eval`:** Heads a list of forms which should be evaluated.
-   **`:check`:** Heads a list of criteria which should be checked.
-   **`:failcheck`:** If checks to this point have generated any errors or failures,
    then the `process` criterion is aborted.
-   **`:errcheck`:** If checks to this point have generated any errors (but not
    failures), then the `process` criterion is aborted.

The `:process` criterion takes no value arguments in a `def-test`.

For example:

    (def-test process-1
        (:process (:eval (setf zzz 0))
                  (:check (:true-form (eql zzz 0)))
                  (:eval (incf zzz))
                  (:check (:true-form (eql zzz 1)))
                  (:eval (incf zzz))
                  (:check (:true-form (eql zzz 2)))))

# Testing invariants against sampled data<a id="orgheadline53"></a>

The `sample` criterion provides random generation of data for
validating program properties.  Our approach is based on Claessen and
Hughes's Quickcheck (Koen Claessen and John Hughes, \`\`QuickCheck: a
lightweight tool for random testing of Haskell programs,'' from
*Proceedings of the International Conference on Functional
Programming*, 2000.  QuickCheck papers, code and other resources are
available at [www.cs.chalmers.se/~rjmh/QuickCheck](http://www.cs.chalmers.se/~rjmh/QuickCheck)).

This style of testing is somewhat more complicated than specific tests
on single, bespoke forms.  There are two distinct efforts, which we
address in the next two sections: describing how the sample data is to
be generated, and specifying the test itself.

## Generating sample data<a id="orgheadline51"></a>

Data generation is centered around the generic function `arbitrary`.

This function takes a single argument, which determines the
type of the value to be generated.  For simple types, the name of the type (or
the class object, such as returned by `find-class` by itself is a complete
specification.  For more complicated types, `arbitrary` can also take a list
argument, where the first element gives the type and the remaining elements are
keyword argument providing additional requirements for the generated value.

NST provides methods of *arbitrary* for many standard Lisp types.
Non-scalar types are associated with additional keyword arguments for
constraints on value generation.

-   Standard numeric types `number`, `real`, `rational`, `integer`,
    `float`, `fixnum`, `bignum`, `ratio`, `short-float`, `single-float`,
    `double-float`, `long-float`, `complex`.
    
    These standard numeric types are not associated with additional
    keyword arguments.
    
        (nst:arbitrary t)
        (nst:arbitrary 'complex)
        (nst:arbitrary 'integer)
        (nst:arbitrary 'ratio)
        (nst:arbitrary 'single-float)
    
    Note that `short-float`, `single-float`, `double-float` are not
    available on Allegro Lisp.

-   Types `character` and `string`:
    -   Argument `noncontrol`.  Excludes the control characters associated
        with ASCII code 0 through 31.
    -   Argument `range`.  Allows the range of characters to be restricted
        to a particular subset:
        
        <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
        
        
        <colgroup>
        <col  class="org-left" />
        
        <col  class="org-left" />
        </colgroup>
        <thead>
        <tr>
        <th scope="col" class="org-left">Value</th>
        <th scope="col" class="org-left">Meaning</th>
        </tr>
        </thead>
        
        <tbody>
        <tr>
        <td class="org-left">`:standard`</td>
        <td class="org-left">Codes up to 96</td>
        </tr>
        
        
        <tr>
        <td class="org-left">`:ascii`</td>
        <td class="org-left">Codes through 127</td>
        </tr>
        
        
        <tr>
        <td class="org-left">`:ascii-ext`</td>
        <td class="org-left">Codes through 255</td>
        </tr>
        </tbody>
        </table>
        
        Omitted or with any other value, characters with any code up to
        `char-code-limit` can result.  Examples:
        
            (nst:arbitrary 'character)
            (nst:arbitrary '(character :noncontrol t
                                       :range :standard))
-   Type `symbol`.
    -   Argument `existing`. If non-nil, requires that the result be a
        previously-interned symbol.
    -   Argument `exported`. Requires that the result be not only a
        previously-interned symbol, but also one exported by its package.
        Ignored if `existing` is explicitly set to nil.
    -   Argument `package`. Specifies the package from which the symbol
        will be generated.  If omitted, a package is selected at random
        from the existing ones.
    -   Argument `nonnull`. If non-nil, allows `arbitrary` to ignore other
        restriction to guarantee returning a non-nil symbol.  When null,
        `arbitrary` may return nil.
    -   Argument `gensym`. If non-nil, and if `arbitrary` is explicitly
        set to nil, returns a new uninterned symbol.

-   Type `scalar`.  The common supertype of the above numeric types,
    `character`, `string` and `symbol`.  This specifier is not
    associated with additional keyword arguments.

-   Type `cons`.
    -   Arguments `car` and `cdr` should be additional type
        specifications, used direct the generation of respectively the left
        and right elements of the result.  Each defaults to `t`.

-   Type `list` and
    `vector`.
    -   Argument `length` specifies the length of the structure. If
        omitted, will be randomly generated.
    -   Argument `elem` directs the generation of the container's
        elements.  For both, the default element type is `t`.

-   Type `array`.
    -   Argument `elem`. As for `list` and `vector`.
    -   Argument `dimens`.  Should be a list of nonnegative integers
        specifying the length of each dimension of the array.  If omitted,
        will be randomly generated.
    -   Argument `rank`. Specifies the number of dimensions. If omitted
        but `dimens` is given, will be set to the length of `dimens`.  If
        both `rank` and `dimens` are omitted, then both are randomly
        generated.

-   Type `hash-table`.
    -   Argument `size`. Specifies the number of entries in the table.  If
        omitted, will be randomly generated.
    -   Argument `test`. Specifies the hash table's test function.  If
        omitted, will be randomly selected from `eq`, `eql`, `equal` and
        `equalp`.
    -   Arguments `key` and `val` direct the generation of the table's
        keys and values, respectively.  For the keys, the default element
        type is `t` when the test function is texttt{eq} or `eql`, and
        `scalar` otherwise.  For the values, the default element type is
        `t`.

-   Type `t`.  The common supertype of all Lisp types.  This specifier
    is not associated with additional keyword arguments.

Beyond those standard Lisp types, NST provides the type `scalar` as a
supertype of the numeric types plus character, `string` and
`symbol`. Users may extend this definition to include
additional type specifications, as we discuss below.  Types are not
associated with `scalar` are referred to as `compound`
(although there is no corresponding type specification).  To avoid
generating structures too large to hold in memory, NST provides the
global variable `*max-compound-structure-depth*` and the macro
`compound-structure`.

The `*max-compound-structure-depth*` variable sets the maximum nesting depth
of compound data structures: beyond that depth, `scalar` rather than `t` is the
default element generator.  This restriction does not apply to explicitly
specified element types, only to the use of defaults.

The `compound-structure` macro wraps substructure which should be considered
compound for the limits set by `*max-compound-structure-depth*`.

New type specifications for invariant-testing. are defined with the
`def-arbitrary-instance-type` macro.

    (def-arbitrary-instance-type ( SPEC-NAME [ :params FORMALS ]
                                             [ :scalar BOOL ]
                                             [ :key KEY ] )
      FORM
      FORM
      ...
      FORM)

-   **`formals`:** Formal parameter definition used to pass subcomponent types.
-   **`scalar`:** When a non-null value is provided for the `:scalar` argument, the
    new specifier is taken to be generable by the `:scalar`
    specification.
    
        (def-arbitrary-instance-type (ratio :scalar t)
          (/ (arbitrary 'integer)
             (let ((raw (arbitrary (find-class 'integer))))
               (cond
                 ((< raw 0) raw)
                 (t (+ 1 raw))))))
-   **`key`:** The `:key` argument gives a list of keyword arguments which may
    accompany the new specification.  For the `cons` type, keyword
    arguments allow specifications for the left and right components:
    
        (def-arbitrary-instance-type
          (cons :key ((car t car-supp-p) (cdr t cdr-supp-p)))
          (compound-structure
           (when (and (not car-supp-p)
                      (>= *current-compound-structure-depth*
                          *max-compound-structure-depth*))
             (setf car 'scalar))
           (when (and (not cdr-supp-p)
                      (>= *current-compound-structure-depth*
                          *max-compound-structure-depth*))
             (setf cdr 'scalar))
           (cons (arbitrary car) (arbitrary cdr))))
-   **`form`:** Construct and return (as if through `progn` the arbtrary instance.

## Invariants as tests<a id="orgheadline52"></a>

# Defining test criteria<a id="orgheadline58"></a>

The criteria used in test forms decide whether, when and how to use
the forms under test and the forms and subcriteria provided to each
test criterion.  Criteria receive their arguments as forms, and may
examine them with or without evaluation, as the particular criterion
requires.  NST provides two mechanisms for defining new criteria, and
a number of support functions for use within these definitions.  The
simpler, but more limited, way to define a new criterion is by
specifying how it should be rewritten to another criterion.  The
`def-criterion-alias` macro provides this mechanism, which we discuss
in Section \ref{def-criterion-alias-section}.  The `def-criterion`
macro provides the more general mechanism for criteria definition,
where Lisp code produces a result report from the forms under test and
criterion's forms and subcriteria.  We discuss `def-criterion` in
Section \ref{def-criterion-section}.  We discuss the NST API for
creating these result reports in Section
\ref{criteria-forms-report-section}, and for recursive processing of
subcriteria in Section \ref{subcriteria-section}.

## Aliases over criteria<a id="orgheadline54"></a>

The simplest mechanism for defining a new criterion involves simply
defining one criterion to rewrite as another using `def-criterion-alias`.

    (def-criterion-alias (name (:seq arg))
      [ doc-string ]
      expansion)

The body of the expansion should be a Lisp form which, when evaluated, returns
an S-expression quoting the new criterion which the rewrite should produce.  The
`arg` are passed as for Lisp macros: they are not evaluated and are most
typically comma-inserted into a backquoted result.  For example:

    (def-criterion-alias (:forms-eq) `(:predicate eq))
    (def-criterion-alias (:symbol name) `(:eq ',name))

## Reporting forms<a id="orgheadline55"></a>

NST provides functions both for building test reports, and for adding
information to a report.

The `make-success-report` function indicates a successful test result.

    (make-success-report)

Note that some older examples show `(make-check-result)`, `(emit-success)` or
`(check-result)`.  The former is an internal function and should not be used
from outside the core NST files.  The latter two are deprecated.

The `make-failure-report` function returns a report of test failure.

    (make-failure-report FORMAT ARGS)

The `format-string` and `args` are as to the Common Lisp function `format`.  The
`emit-failure` function is an older, deprecated version of this function.

Function `make-warning-report` is like `make-failure-report`, but provides
supplimentary information as a warning.

    (make-warning-report [ :format FORMAT-STRING ] [ :args ARG-FORM-LIST ])

The `emit-warning` function is an older, deprecated version of this function.

Function `make-error-report` produces a report of an error during test execution.

    (make-error-report [ :format FORMAT-STRING ] [ :args ARG-FORM-LIST ] )

For use within user-defined NST criteria: add an error to a result.

    (add-error RESULT-REPORT [ :format FORMAT-STRING ] [ :args ARGUMENT-LIST ] )

For use within user-defined NST criteria: add a failure to a result.

    (add-failure RESULT-REPORT [ :format FORMAT-STRING ] [ :args ARGUMENT-LIST ])

For use within user-defined NST criteria: add an info note to a result.

    (add-info RESULT-REPORT INFO-ITEM)

For use within user-defined NST criteria: add a warning to a
result.

The `add-warning` function adds an warning to a result record.  The item can be
any of a Lisp warning, an NST check-note or a format string; in the first two
cases, no additional arguments should be provided.

    (add-warning RESULT-REPORT [ :format ITEM ] [ :args ARGUMENT-LIST ] )

The helper function `wrap-thrown-lisp-warning` creates an NST `check-note`
object from a standard Lisp `warning`.

## Processing subcriteria<a id="orgheadline56"></a>

The criterion itself can contain *subcriteria* which can be
incorporated into the main criterion's assessment.  NST provides two
functions which trigger testing by a subcriterion, each returning the
check's result report.

The `check-criterion-on-value` function can be called from within a criterion
body to verify that a value adheres to a criterion.

    (check-criterion-on-value CRITERION VALUE)

This function verifies that the values return by evaluating the form adheres
to the criterion.

    (check-criterion-on-form CRITERION FORM)

## General criteria definitions<a id="orgheadline57"></a>

The `def-criterion` macro defines a new criterion for use in NST tests.
These criteria definitions are like generic function method definitions with two
sets of formal parameters: the forms provided as the actual parameters of the
criterion itself, and the values arising from the evaluation of the forms under
test.

    (def-criterion (name criterion-lambda-list values-lambda-list)
      [ doc-string )
      form
      form
      ...
      form)

-   **name:** Name of the criterion.
-   **criterion-lambda-list:** Lambda list for the arguments to the criterion.  Optionally, the first element of the list is a symbol specifying the parameter-passing semantics for the criterion arguments: `:values` for call-by-value, or `:forms for call-by-name (the default).  The list may include the keywords =&key`, `&optional`, `&body` and `&rest` but may not use `&whole` or `&environment`.  Apart from this restriction, in the former case the list may be any ordinary lambda list as for `defun`, and in the latter case the list may be any macro lambda list as for `defmacro`.
-   **values-lambda-list:** Lambda list for the forms under test.  Optionally, the first element of the list is a symbol specifying the parameter-passing semantics for the criterion arguments: :values for call-by-value (the default), or :form for call-by-name.  In the former case, the list may include the keywords `&key`, `&optional`, `&body` and `&rest`, but not `&whole` or `&environment`; apart from that restriction, list may be any ordinary lambda list as for `defun`.  In the latter case, the remainder of the list must contain exactly one symbol, to which a form which would evaluate to the values under test will be bound.
    If the criterion ignores the values, then instead of a lambda list, this argument may be the symbol `:ignore`.  On many platforms, listing a dummy parameter which is then `declare=d =ignore` or `ignorable` will produce a style warning: the body of a `def-criterion` should not be assumed to correspond directly to the body of a `defmethod`; in general there will be surrounding =destructuring-bind=s.
-   **documentation:** An optional documentation string for the criterion.
-   **form:** The body of the criterion definition should return a test result report contructed with the `make-success-report`, etc. functions.

Examples:

    (def-criterion (:true () (bool))
      (if bool
          (make-success-report)
          (make-failure-report :format "Expected non-null, got: ~s"
                        :args (list bool))))
    
    (def-criterion (:eql (target) (actual))
      (if (eql (eval target) actual)
          (make-success-report)
          (make-failure-report :format "Not eql to value of ~s"
                        :args (list target))))

# The runtime system<a id="orgheadline70"></a>

The runtime system provides several operations for scheduling and
running tests, and debugging failing and erring tests.

User-level NST operations are accessible from the REPL via the `nst-cmd`
macro.

    (nst-cmd NST-COMMAND ARG ... ARG)

Where a particular system supports the facility (Currently Allegro, and SBCL
under ACL-REPL) the top-level alias `:nst` provides a shorthand to this
function.
Calling `nst` or `nst-cmd` without a command argument repeats the last
test-executing command.
For the sake of brevity we use the `:nst` shorthand below.

## Command `help`<a id="orgheadline59"></a>

The `:help` command gives a complete inventory of runtime system
commands.

Usage:

    :nst :help

There are a number of commands for running tests, but most of the time
only one will be needed:

## Command `run`<a id="orgheadline60"></a>

The `:run` command executes all tests in the named package, or in the
named group, or runs the named test. It is not necessary to prefix the name
with a package prefix.  The name does not need to be prefix-qualified, but if
the name is ambiguous then `:run` will simply report the possible
interpretations.

Usage:

    :nst :run NAME

## Command `run-package`<a id="orgheadline61"></a>

The `:run-package` command executes all tests associated with groups in
the named packages, and reports the test results afterwards.

Usage:

    :nst :run-package (:SEQ PACKAGE)

## Command `run-group`<a id="orgheadline62"></a>

The `:run-group` command executes all tests associated with the name
groups, and reports the test results afterwards. The group name should be
package-qualified.

Usage:

    :nst :run-group (:SEQ GROUP)

## Command `run-test`<a id="orgheadline63"></a>

The `:run-test` command executes the given test.  Both the group and test
name should be package-qualified.

Usage:

    :nst :run-test GROUP TEST

One further command for running a test is useful when writing and
debugging the tests themselves:

## Command `apply`<a id="orgheadline64"></a>

The `:apply` command assesses whether a test criterion prints the uses to
which a particular name has been applied in an NST session.

Usage:

    :nst :apply NAME

-   The `apply` criterion first evaluates the forms under test, applying
    `FUNCTION` to them.  The overall criterion passes or fails exactly
    when the subordinate `CRITERION` with the application's multiple
    result values.
    
        (:apply FUNCTION CRITERION)
    
    Example:
    
        (def-test applycheck (:apply cadr (:eql 10)) '(0 10 20))

There are two commands for (re)printing the results of tests:

## Command `report`<a id="orgheadline65"></a>

The `:report` command summarizes successes, failures and errors in tests.
It reports either for the named artifact, or for all recently-run tests.

Usage:

    :nst :run
    :nst :run PACKAGE
    :nst :run GROUP
    :nst :run GROUP TEST

## Command `detail`<a id="orgheadline66"></a>

The `:detail` command gives detailed information about individual test
results.

Usage:

    :nst :detail
    :nst :detail PACKAGE
    :nst :detail GROUP
    :nst :detail GROUP TEST

The `set` and `unset` display and adjust NST's configuration.

## Command `set`<a id="orgheadline67"></a>

The `:set` command assigns or displays the values of NST runtime
switches.

Usage:

    :nst :set PROPERTY
    :nst :set PROPERTY VALUE

There are currently three properties which can be manipulated by `set`
and `unset`:

The `:verbosity` switch controls the level of NST's output.

Usage:

    :nst :verbose SETTING

Valid settings are:

-   `:silent` (aka `nil`)
-   `:quiet` (aka `:default`)
-   `:verbose` (aka `t`)
-   `:vverbose`
-   `:trace`

The `:report` and `:detail` commands operate by setting minimum levels of
verbosity.

The `:debug-on-error` switch controls NST's behavior on errors.  When
non-nil, NST will break into the debugger when it encounters an error.

Usage:

    :nst :debug-on-error FLAG

The `:debug` command is a short-cut for setting this property.

The `:debug-on-fail` switch controls NST's behavior when a test fails
When non-nil, NST will break into the debugger when it encounters a failing
test.

Usage:

    :nst :debug-on-fail FLAG

This behavior is less useful than it may seem; by the time the results of the
test are examined for failure, the stack from the actual form evaluation will
usually have been released.  Still, this switch is useful for inspecting the
environment in which a failing test was run.

Note that both `:debug-on-error` and `:debug-on-fail` apply in the case of an
error; if the latter is set but the former is not, then the debugger will be
entered after an erring test completes.

The `:debug` command is a short-cut for setting this property.

## Command `unset`<a id="orgheadline68"></a>

The `:unset` command clears the values of NST runtime switches.

Usage:

    :nst :unset PROPERTY

The `:backtraces` switch, when non-=nil=, directs NST to attempt to
capture the Lisp backtrace of errors in tests.

Usage:

    :nst :backtraces FLAG

This property is only available on platform which allow programmatic examination
of backtraces, which is not standardized in Common Lisp; currently we have
implemented this feature on Allegro only.

This property has a complicated default setting.  Firstly, if the symbol
='common-lisp-user::\*nst-generate-backtraces\*= is bound when NST loads, NST will
use its value as the initial value for this property.  Otherwise by default, on
MacOS systems the property initializes to `nil` because of a known error on that
system, but this setting can be overriden by the property
`:nst-unsafe-allegro-backtraces`.  Finally, if none of these issues apply, the
initial value is `t`.

This property is only available on platform which allow programmatic
examination of backtraces, which is not standardized in Common Lisp;
currently we have implemented this feature on Allegro only.This
property has a complicated default setting.  Firstly, if the symbol
='common-lisp-user::\*nst-generate-backtraces\*= is bound when NST
loads, NST will use its value as the initial value for this
property.  Otherwise by default, on MacOS systems the property
initializes to `nil` because of a known error on that system, but
this setting can be overriden by the property
`:nst-unsafe-allegro-backtraces`.  Finally, if none of these issues
apply, the initial value is ==.

The above NST commands are governed by a number of global
variables. In general, interactive use of NST should not require
direct access to these variables, but when automating NST operations
may require changing, or creating a new dynamic scope for, their
settings.

User variable `*debug-on-error*`: if non-nil, will break into the Lisp REPL
debugger upon encountering an unexpected error.  If nil, will record the error
and continue with other tests.

User variable `*debug-on-fail*`: if non-nil, will break into the Lisp REPL
debugger upon encountering a test which fails.  If nil, will record the failure
and continue with other tests.  This variable is useful inspecting the dynamic
environment under which a test was evaluated.

User variable `*default-report-verbosity*` determines the default value for
`*nst-verbosity*` when printing reports (2 by default).

Determines the output stream to which NST should print its output
(`*standard-output*` by default).

## Command `open`<a id="orgheadline69"></a>

The `:open` command injects the binding given by the `fixture` set
into the current package.

Usage:

    :nst :open (:SEQ FIXTURE)

Example:

    CL-USER(75): (nst:def-fixtures small-fixture ()
                    (fix-var1 3)
                    (fix-var2 'asdfg))
    NIL
    CL-USER(76): (boundp 'fix-var1)
    NIL
    CL-USER(77): :nst :open small-fixture
    Opened fixture SMALL-FIXTURE.
    CL-USER(78): fix-var1
    3
    CL-USER(79):

Fixtures can be opened into a different package than where they were
first defined, but these bindings are in addition to the bindings in
the original package, and are made by a symbol import to the
additional package.

# Integration with ASDF<a id="orgheadline75"></a>

NST's integration with ASDF is a work in progress.  This section
described the current integration, the ways we expect it to change,
and a less-flexible and lower-level, but likely more stable,
alternative integration technique.

## NST's ASDF systems<a id="orgheadline73"></a>

From version 1.2.2, the system `:asdf-nst` provides two classes
for ASDF system definitions, `asdf:nst-test-runner` and
`asdf:nst-test-holder`.

Up to NST 1.2.1 `:asdf-nst` provided a single class
`asdf:nst-testable`, and in the future we plan to reunify the
current two classes into a single class again.  However our first
implementation required NST to be loaded even when a system was
*not* being tested, because we had no way to distinguish the
source code associated with testing from production code.  We plan to
solve this problem with a new file type `nst-file` in a future
version of NST.  This file type *not* be compiled or loaded
for the `compile-op` or `load-op` of the system, only
for its `test-op`.

### Test-running systems<a id="orgheadline71"></a>

ASDF systems of the `asdf:nst-test-runner` class do not
themselves contain NST declarations in their source code, but may
identify other systems which do, and which should be tested as a part
of testing the given system.  These systems also allow local
definitions of NST's configuration for the execution of their tests.

Specify that a system runs NST tests by providing `:class
  asdf:nst-test-runner` argument to `asdf:defsystem`.  Use the
`:nst-systems` argument to name the systems which house the
actual unit tests:

-   `:nst-systems (system system ... system)`
    
    Specifies a list of other systems
    which should be tested when testing this system.  These other
    systems *not* otherwise need to be identified as a
    dependency of this system (nor, for that matter, does `:nst`
    itself); they will be loaded upon `test-op` if they are not
    yet present.

Another optional argument to an `nst-test-runner` system
definition is:

-   `:nst-init (arg-list ... arg-list)`
    
    Initializing arguments to NST, to be executed after this system is
    loaded.  Each `arg-list` is passed as the arguments as if to
    a call to the `nst-cmd` macro.

-   `:nst-debug-config form`
    
    NST debugging customization for this system.  The `FORM` Should be
    an expression which, when evaluated, returns a list of keyword
    arguments; note that to give the list itself, it must be explicitly
    quoted, *which is a change of behavior from pre-1.2.2 versions*.

-   `:nst-debug-protect (symbol ... symbol)` \par
    Gives a list of variables whose values should be saved before
    applying any configuration changes from `:nst-debug-config`,
    and restored after testing.

-   `:nst-push-debug-config t-or-nil`
    
    If non-nil, then when this system is loaded its `:nst-debug` and
    `:nst-debug-protect` settings will be used as NST's defaults.

### Test-containing systems<a id="orgheadline72"></a>

The `asdf:nst-test-holder` class is a subclass of
`nst-test-runner` for systems which are not only tested via
NST, but also contains NST tests in their source code.

Specify that a system defines NST tests by providing `:class
  asdf:nst-test-holder` to `asdf:defsystem`.  The arguments for
`asdf:nst-test-runner` may be used for
`asdf:nst-test-holder`, as well as the following:

-   `:nst-packages (package package ... package)`
    
    When the system is tested, all
    groups and tests in the named packages should be run.

-   `:nst-groups ((package group) ... (package group))`
    
    When the system is tested, tests in the named groups should be run.
    Naming the package separately from the group and test in this
    argument (and in the similar arguments below) allows the group to be
    named before its package is necessarily defined.

-   `:nst-tests ((package group test) ... (package group test))`
    
    When the system is tested, all the named
    tests should be run.

The next three arguments to an `nst-testable` system are mutually
exclusive, and moreover exclude any of the above group or
`:nst-systems`:

-   `:nst-package package`
    
    When the system is
    tested, all groups and tests in the named package should be run.

-   `:nst-group (package group)`
    
    When the system is tested, all tests in the named group should be
    run.

-   `:nst-test (package group test)`
    
    When the system is tested, the given test should be run.

Examples of `nst-testable` ASDF system definitions:

    ;; NST and its ASDF interface must be loaded
    ;; before we can process the defsystem form.
    (asdf:oos 'asdf:load-op :asdf-nst)
    #+LaTeX:
    (defsystem :mnst
        :class nst-test-holder
        :description "The NST test suite's self-test."
        :serial t
        :nst-systems (:masdfnst)
        :nst-groups ((:mnst-simple . g1)
                     (:mnst-simple . g1a)
                     (:mnst-simple . g1a1)
                     (:mnst-simple . core-checks))
        :depends-on (:nst)
        :in-order-to ((test-op (load-op :mnst)))
        :components ((:module "core"
                      :components ((:file "byhand")
                                   (:file "builtin-checks")))))

## An alternate ASDF integration technique<a id="orgheadline74"></a>

We plan to deprecate and then remove `asdf:nst-test-holder` and
`nst-test-runner` once we have implemented a unified
replacement for them.  To avoid the possibility of a bit-rotted test
scheme, the link between a system and its unit tests can be made
explicit by providing methods for ASDF generic functions which make
calls to the NST API.  Specifically:

-   A method of the ASDF `asdf:perform` generic function
    specialized to the `asdf:test-op` operation and the system in
    question will be executed to test a system.  So an appropriate
    method definition would begin:
    
        (defmethod asdf:perform ((op asdf:test-op)
                                 (sys (eql (asdf:find-system
                                              :SYSTEM-NAME))))

-   NST API functions for running tests are:
    -   `nst:run-package`
    
    -   `nst:run-group`
    
    -   `nst:run-test`

-   The main NST API function for printing the results of testing is
    `asdf:report-multiple`\\,.  In situations where only a single
    package, group or test is associated with a system, one of the
    following function may be more convenient:
    -   `nst:report-package`
    
    -   `nst:report-group`
    
    -   `nst:report-test`

When providing an explicit `asdf:perform` method, it is also necessary
to explicitly list system dependencies to NST and to the other systems
which contain the tested system's unit test definitions.

# Inheritance-based test methods<a id="orgheadline80"></a>

/This feature is in-progress. It currently does not work under
Lispworks or Clisp, and details of the API may change in subsequent
versions./

For testing objects in a class hierarchy NST offers xUnit-style test
methods dispatching on different classes.  The idea is that an object
should have all relevant tests applied to it without requiring that
the tests be explicitly enumerated in the test definition: all tests
applicable to an object's class, or to any of its superclasses, should
be discovered and run.

Our running examples of this section are tests on objects of these
four classes:

    (defclass top-cls ()
         ((tc1 :initarg :tc1 :reader tc1)
          (tc2 :initarg :tc2 :reader tc2)))
    
    (defclass mid-cls (top-cls)
         ((mc1 :initarg :mc1 :reader mc1)
          (mc2 :initarg :mc2 :reader mc2)))
    
    (defclass side-cls ()
         ((sc1 :initarg :sc1 :reader sc1)
          (sc2 :initarg :sc2 :reader sc2)))
    
    (defclass bot-cls (mid-cls side-cls)
         ((bc1 :initarg :bc1 :reader bc1)
          (bc2 :initarg :bc2 :reader bc2)))

## Declaring methods<a id="orgheadline76"></a>

There are two macros which define a particular method of a generic
test function.

The `def-test-method-criterion` macro provides a simple facility for
defining a generic test function method in terms of an NST criterion.

    (def-test-method-criterion function-name class-name
      criterion)

-   **function-name:** The name of the test function for which we are
    defining a method.
-   **class-name:** The class for which we are defining a method.
-   **criterion:** The criterion to be applied to members of the class.

For example:

    (nst:def-test-method-criterion for-clses top-cls
          (:predicate (lambda (tc) (< (tc1 tc) (tc2 tc)))))

The `def-test-generic` declares a generic test function.

    (def-test-generic function-name)

For example,

    (nst:def-test-generic for-clses)

The `def-test-method` defines a general method for a generic test
function.

    (def-test-method function-name
                     ( test-value class-name )
      form
      ...
      form)

-   **function-name:** The name of the test function for which we are defining a method.
-   **test-value:** Formal parameter to which the value under test will be bound.
-   **class-name:** The class for which we are defining a method.

The method body should return a test result report, constructed with
`make-success-result`, etc.For example:

    (nst:def-test-method for-clses (o mid-cls)
      (with-slots (mc1 mc2) o
        (cond
          ((< mc1 mc2) (make-success-report))
          (t (make-failure-report :format "~d not < ~d" :args (list mc1 mc2))))))
    (nst:def-test-method for-clses (o side-cls)
      (with-slots (sc1 sc2) o
        (cond
          ((eql sc1 sc2) (make-success-report))
          (t (make-failure-report :format "~d not eql ~d" :args (list sc1 sc2))))))

## Invoking methods<a id="orgheadline78"></a>

### Criterion `methods`<a id="orgheadline77"></a>

The `:methods` criterion runs the test functions applicable to the value
under test.

For example:

    (def-test-group method-tests ()
      (def-test t-p :methods (make-instance 'top-cls :tc1 0 :tc2 2))
      (def-test m-p :methods (make-instance 'mid-cls :tc1 0 :tc2 2 :mc1 0 :mc2 2))
      (def-test s-p :methods (make-instance 'side-cls :sc1 1 :sc2 1))
      (def-test b-p :methods (make-instance 'bot-cls
                               :tc1 0 :tc2 2 :mc1 0 :mc2 2 :sc1 1 :sc2 1))
      (def-test t-f :methods (make-instance 'top-cls :tc1 4 :tc2 2))
      (def-test m-f-t  :methods (make-instance 'mid-cls
                                  :tc1 4 :tc2 2 :mc1 0 :mc2 2))
      (def-test m-f-m  :methods (make-instance 'mid-cls
                                  :tc1 0 :tc2 2 :mc1 4 :mc2 2))
      (def-test m-f-mt :methods (make-instance 'mid-cls
                                  :tc1 4 :tc2 2 :mc1 4 :mc2 2))
      (def-test s-f :methods (make-instance 'side-cls :sc1 1 :sc2 3))
      (def-test b-f-t :methods (make-instance 'bot-cls
                                 :tc1 4 :tc2 2 :mc1 0 :mc2 2 :sc1 1 :sc2 1))
      (def-test b-f-m :methods (make-instance 'bot-cls
                                 :tc1 0 :tc2 2 :mc1 4 :mc2 2 :sc1 1 :sc2 1))
      (def-test b-f-s :methods (make-instance 'bot-cls
                                 :tc1 0 :tc2 2 :mc1 0 :mc2 2 :sc1 1 :sc2 3))
      (def-test b-f-mt :methods (make-instance 'bot-cls
                                  :tc1 4 :tc2 2 :mc1 4 :mc2 2 :sc1 1 :sc2 1))
      (def-test b-f-ms :methods (make-instance 'bot-cls
                                  :tc1 0 :tc2 2 :mc1 4 :mc2 2 :sc1 1 :sc2 3))
      (def-test b-f-ts :methods (make-instance 'bot-cls
                                  :tc1 4 :tc2 2 :mc1 0 :mc2 2 :sc1 1 :sc2 3))
      (def-test b-f-mts :methods (make-instance 'bot-cls
                                   :tc1 4 :tc2 2 :mc1 4 :mc2 2 :sc1 1 :sc2 3)))

## Method combinations<a id="orgheadline79"></a>

NST defines a method combination `nst-results` as the default method
combination for functions defined by `def-test-generic`.  This
combination runs *all* applicable methods, and combines all of their
results into a single NST result record.

This default can be overridden by specifying `t` as the method
combination in the intial declaration.

    (nst:def-test-generic overridden
        (:method-combination t))
    (nst:def-test-method-criterion overridden mid-cls
      (:slots (mc1 (:eql 0))
              (mc2 (:eql 2))))
    (nst:def-test-method-criterion overridden bot-cls
      (:slots (sc1 (:eql 1))
              (sc2 (:eql 1))))

# Deprecated forms<a id="orgheadline83"></a>

The macros, functions and variables documented in this section are all
deprecated.  Some continue to be exported from the NST API; others
have already been removed.  This section describes how code using
these forms should be ported to the active NST API.

## Older criteria-defining macros<a id="orgheadline81"></a>

The `def-criterion-unevaluated` macro is deprecated as of NST 2.1.2.  It was
consolidated into the `def-criterion` macro.

Replace:

    (def-criterion-unevaluated name (pattern ... pattern) name
      BODY)

with:

    (def-criterion name (:forms pattern ... pattern)
                        (:form name)
                        BODY)

The `def-values-criterion` macro was deprecated as of NST 1.3.0. For new
criteria, use `def-criterion` instead.  In the short term, code using
`def-values-criterion` should continue to work as before.

The `def-form-criterion` macro was deprecated as of NST 1.3.0. *Code using
`def-form-criterion` in any but the simplest ways is very likely to fail.* Use
`def-criterion` instead.

## Old test result generators<a id="orgheadline82"></a>

The `emit-failure` function is deprecated; use `make-failure-report`
instead.

The `emit-success` function is deprecated; use `make-success-report`
instead.

The `emit-warning` function is deprecated; use `make-warning-report`
instead.
