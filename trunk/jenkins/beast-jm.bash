#!/bin/bash

export NST_DIR=~/Lib/Lisp/SIFT/nst/
export CLOSER_MOP_DIR=~/Lib/Lisp/SIFT/closer-mop/

##  #################################################################
##  The lines below can be copied in to the jenkins script box on
##  beast.  Do NOT use the NST_DIR and CLOSER_MOP_DIR above; they will
##  fail!
##  #################################################################

export NST_ALLEGRO_MIXEDCASE=/usr/local/acl/acl82/mlisp
export NST_ALLEGRO_UPCASE=/usr/local/acl/acl82/alisp
export NST_ALLEGRO_MIXEDCASE8=/usr/local/acl/acl82/mlisp8
export NST_ALLEGRO_UPCASE8=/usr/local/acl/acl82/alisp8
export NST_SBCL_UPCASE=/usr/local/bin/sbcl

export SKIP_NST_CLISP_UPCASE=1
export NST_CLISP_UPCASE=
export SKIP_NST_CLISP_MIXEDCASE=1
export NST_CLISP_MIXEDCASE=
export SKIP_NST_CCL_UPCASE=1
export NST_CCL_UPCASE=
export SKIP_NST_LW_UPCASE=1
export NST_LW_UPCASE=

#################################################################

./jenkins.perl