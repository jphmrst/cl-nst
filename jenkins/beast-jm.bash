#!/bin/bash

export NST_DIR=/home/jmaraist/Lib/Lisp/nst/
export CLOSER_MOP_DIR=/home/jmaraist/Lib/Lisp/SIFT/closer-mop/

##  #################################################################
##  The lines below can be copied in to the jenkins script box on
##  beast.  Do NOT use the NST_DIR and CLOSER_MOP_DIR above; they will
##  fail!
##  #################################################################

export SKIP_NST_ALLEGRO_MIXEDCASE=/usr/local/acl/acl82/mlisp
export NST_ALLEGRO_MIXEDCASE=/usr/local/acl/acl82/mlisp
export SKIP_NST_ALLEGRO_UPCASE=/usr/local/acl/acl82/alisp
export NST_ALLEGRO_UPCASE=/usr/local/acl/acl82/alisp
export SKIP_NST_ALLEGRO_MIXEDCASE8=/usr/local/acl/acl82/mlisp8
export NST_ALLEGRO_MIXEDCASE8=/usr/local/acl/acl82/mlisp8
export SKIP_NST_ALLEGRO_UPCASE8=/usr/local/acl/acl82/alisp8
export NST_ALLEGRO_UPCASE8=/usr/local/acl/acl82/alisp8
export SKIP_NST_SBCL_UPCASE=/usr/local/bin/sbcl
export NST_SBCL_UPCASE=/usr/local/bin/sbcl

export NST_CCL_UPCASE=/home/jmaraist/Lib/Lisp/clozure/ccl/lx86cl
export NST_CCL64_UPCASE=/home/jmaraist/Lib/Lisp/clozure/ccl/lx86cl64

export SKIP_NST_CLISP_UPCASE=1
export NST_CLISP_UPCASE=
export SKIP_NST_CLISP_MIXEDCASE=1
export NST_CLISP_MIXEDCASE=
export SKIP_NST_LW_UPCASE=1
export NST_LW_UPCASE=

#################################################################

./jenkins.perl
