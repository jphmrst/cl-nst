
Current using simple-mnst.lisp.

:pa mnst-simple
(nst::run-package)
(nst::report-package)

 - Print contexts and stacks on test asks.
 - The ones with info.

Open tickets:

  #37  Refine printing of test details --- contexts, expression stacks, info
  #32  Redesign interaction schemes
  #36  Document class/method setup
  #33  Add SBCL compatibility
  #38  Add ASDF compatibility
  #39  Anonymous fixtures are broken

Also need to comment and doc-comment everything that's in place.

[August 2008]
Working now on the status reporting.  Want something in place that
shows results even if it's not perfect.

[July 2008]
The order of calls is now correct for both groups and standalone
tests.

[April 2008]
The order of calls is now correct for groups, but not standalone
tests.

