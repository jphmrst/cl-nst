#!/bin/bash
export NST_DIR=/home/jm/Lib/Lisp/nst/
export CLOSER_MOP_DIR=/home/jm/Lib/Lisp/SIFT/closer-mop/

# export SKIP_NST_ALLEGRO_MIXEDCASE=`which mlisp`
export NST_ALLEGRO_MIXEDCASE=`which mlisp`

# export SKIP_NST_ALLEGRO_UPCASE=`which alisp`
export NST_ALLEGRO_UPCASE=`which alisp`

# export SKIP_NST_ALLEGRO_MIXEDCASE8=`which mlisp8`
export NST_ALLEGRO_MIXEDCASE8=`which mlisp8`

# export SKIP_NST_ALLEGRO_UPCASE8=`which alisp8`
export NST_ALLEGRO_UPCASE8=`which alisp8`

# export SKIP_NST_SBCL_UPCASE=`which sbcl`
export NST_SBCL_UPCASE=`which sbcl`

# export SKIP_NST_CLISP_UPCASE=1
export NST_CLISP_UPCASE=`which clisp`

# export SKIP_NST_CLISP_MIXEDCASE=1
export NST_CLISP_MIXEDCASE=`which clisp`

# export SKIP_NST_CCL_UPCASE=1
export NST_CCL_UPCASE=~/Lib/Lisp/clozure/ccl/lx86cl

export SKIP_NST_LW_UPCASE=1
export NST_LW_UPCASE=/usr/local/lib/LispWorksPersonal/lispworks-personal-6-0-1-x86-linux

./jenkins.perl