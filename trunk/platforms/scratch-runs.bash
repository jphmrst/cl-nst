#!/bin/bash

echo Testing JUnit writing on Allegro CL
mkdir -p acl-junit
echo > acl-junit/acl-trunk.log
export NSTJUNITDIR=acl-junit/
mlisp < junit-writer.lisp 2>> acl-junit/acl-trunk.log >> acl-junit/acl-trunk.log

echo Testing JUnit writing on SBCL
mkdir -p sbcl-junit
echo > sbcl-junit/sbcl-trunk.log
export NSTJUNITDIR=sbcl-junit/
sbcl < junit-writer.lisp 2>> sbcl-junit/sbcl-trunk.log >> sbcl-junit/sbcl-trunk.log
