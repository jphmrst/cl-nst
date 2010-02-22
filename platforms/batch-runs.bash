#!/bin/bash

./setup-inputs.bash
mkdir -p logs

# echo Testing the branch on ECL
# echo > logs/ecl-branch.log
# ecl -load branch-pushes.lisp 2>> logs/ecl-branch.log >> logs/ecl-branch.log
# grep TOTAL logs/ecl-branch.log | tail -1

# echo Testing the trunk on ECL
# echo > logs/ecl-trunk.log
# ecl -load trunk-pushes.lisp 2>> logs/ecl-trunk.log >> logs/ecl-trunk.log
# grep TOTAL logs/ecl-trunk.log | tail -1

# ------------------------------------------------------------


# Clear out CLISP FASLs
rm -rf /home/jm/Lib/Lisp/fasl/clisp/*/*

echo Testing the trunk on CLISP
echo > logs/clisp-trunk.log
clisp -i inputs/clisp/init.lisp inputs/clisp/nst-tests.lisp \
    2>> logs/clisp-trunk.log >> logs/clisp-trunk.log
grep TOTAL logs/clisp-trunk.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on CLISP
mkdir -p clisp-junit
rm -f clisp-junit/*
export NSTJUNITDIR=clisp-junit/
clisp -i inputs/clisp/init.lisp inputs/clisp/junit.lisp \
    2>> clisp-junit/clisp-trunk.log >> clisp-junit/clisp-trunk.log
echo Files in $NSTJUNITDIR directory: expected 15, have \
    `ls -1 $NSTJUNITDIR | wc -l`



# Clear out Clozure FASLs
rm -rf /home/jm/Lib/Lisp/fasl/misc/ccl-*/*

echo ------------------------------------------------------------
echo Testing the trunk on Clozure
echo > logs/clozure-trunk.log
~/Lib/Lisp/clozure/ccl/lx86cl --batch < inputs/clozure/nst-tests.lisp \
    2>> logs/clozure-trunk.log >> logs/clozure-trunk.log
grep TOTAL logs/clozure-trunk.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Clozure
mkdir -p clozure-junit
rm -f clozure-junit/*
export NSTJUNITDIR=clozure-junit/
~/Lib/Lisp/clozure/ccl/lx86cl --batch < inputs/clozure/junit.lisp \
    2>> clozure-junit/junit.log >> clozure-junit/junit.log
echo Files in $NSTJUNITDIR directory: expected 15, have \
    `ls -1 $NSTJUNITDIR | wc -l`


# Clear out SBCL FASLs
rm -rf /home/jm/Lib/Lisp/fasl/sbcl/*/*

echo ------------------------------------------------------------
echo Testing the trunk on SBCL
echo > logs/sbcl-trunk.log
sbcl < inputs/sbcl/nst-tests.lisp \
    2>> logs/sbcl-trunk.log >> logs/sbcl-trunk.log
grep TOTAL logs/sbcl-trunk.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on SBCL
mkdir -p sbcl-junit
rm -f sbcl-junit/*
export NSTJUNITDIR=sbcl-junit/
sbcl < inputs/sbcl/junit.lisp \
    2>> sbcl-junit/sbcl-trunk.log >> sbcl-junit/sbcl-trunk.log
echo Files in $NSTJUNITDIR directory: expected 15, have \
    `ls -1 $NSTJUNITDIR | wc -l`


 
# echo Testing the trunk on CMU CL
# echo > logs/cmucl-trunk.log
# cmucl -batch < trunk-asdfind.lisp 2>> logs/cmucl-trunk.log >> logs/cmucl-trunk.log
# grep TOTAL logs/cmucl-trunk.log | tail -1



# Clear out Allegro FASLs
rm -rf /home/jm/Lib/Lisp/fasl/allegro/*/*

echo ------------------------------------------------------------
echo Testing the trunk on Allegro 8.2 mlisp
echo > logs/allegro82-mlisp-trunk.log
mlisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro82-mlisp-trunk.log >> logs/allegro82-mlisp-trunk.log
grep TOTAL logs/allegro82-mlisp-trunk.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Allegro 8.2 mlisp
mkdir -p acl82-mlisp-junit
rm -f acl82-mlisp-junit/*
export NSTJUNITDIR=acl82-mlisp-junit/
mlisp < inputs/acl/junit.lisp \
    2>> acl82-mlisp-junit/acl-trunk.log >> acl82-mlisp-junit/acl-trunk.log
echo Files in $NSTJUNITDIR directory: expected 15, have \
    `ls -1 $NSTJUNITDIR | wc -l`



echo ------------------------------------------------------------
echo Testing the trunk on Allegro 8.2 alisp
echo > logs/allegro82-alisp-trunk.log
alisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro82-alisp-trunk.log >> logs/allegro82-alisp-trunk.log
grep TOTAL logs/allegro82-alisp-trunk.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Allegro 8.2 alisp
mkdir -p acl82-alisp-junit
rm -f acl82-alisp-junit/*
export NSTJUNITDIR=acl82-alisp-junit/
alisp < inputs/acl/junit.lisp \
    2>> acl82-alisp-junit/acl-trunk.log >> acl82-alisp-junit/acl-trunk.log
echo Files in $NSTJUNITDIR directory: expected 15, have \
    `ls -1 $NSTJUNITDIR | wc -l`



echo ------------------------------------------------------------
echo Testing the trunk on Allegro 8.1 mlisp
echo > logs/allegro81-mlisp-trunk.log
mlisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro81-mlisp-trunk.log >> logs/allegro81-mlisp-trunk.log
grep TOTAL logs/allegro81-mlisp-trunk.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Allegro 8.1 mlisp
mkdir -p acl81-mlisp-junit
rm -f acl81-mlisp-junit/*
export NSTJUNITDIR=acl81-mlisp-junit/
/usr/local/allegro/acl81/mlisp < inputs/acl/junit.lisp \
    2>> acl81-mlisp-junit/acl-trunk.log >> acl81-mlisp-junit/acl-trunk.log
echo Files in $NSTJUNITDIR directory: expected 15, have \
    `ls -1 $NSTJUNITDIR | wc -l`



echo ------------------------------------------------------------
echo Testing the trunk on Allegro 8.1 alisp
echo > logs/allegro81-alisp-trunk.log
alisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro81-alisp-trunk.log >> logs/allegro81-alisp-trunk.log
grep TOTAL logs/allegro81-alisp-trunk.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Allegro 8.1 alisp
mkdir -p acl81-alisp-junit
rm -f acl81-alisp-junit/*
export NSTJUNITDIR=acl81-alisp-junit/
/usr/local/allegro/acl81/alisp < inputs/acl/junit.lisp \
    2>> acl81-alisp-junit/acl-trunk.log >> acl81-alisp-junit/acl-trunk.log
echo Files in $NSTJUNITDIR directory: expected 15, have \
    `ls -1 $NSTJUNITDIR | wc -l`
