#!/bin/bash

./setup-inputs.bash
mkdir -p logs

# echo Testing the branch on ECL
# echo > logs/ecl-branch.log
# ecl -load branch-pushes.lisp 2>> logs/ecl-branch.log >> logs/ecl-branch.log
# grep TOTAL logs/ecl-branch.log | tail -1

# echo Testing the trunk on ECL
# echo > logs/ecl-fresh.log
# ecl -load trunk-pushes.lisp 2>> logs/ecl-fresh.log >> logs/ecl-fresh.log
# grep TOTAL logs/ecl-fresh.log | tail -1

# ------------------------------------------------------------


# Clear out CLISP FASLs
rm -rf /home/jm/Lib/Lisp/fasl/clisp/*/*

echo Testing the trunk \(with everything recompiled\) on CLISP
echo > logs/clisp-fresh.log
clisp -i inputs/clisp/init.lisp inputs/clisp/nst-tests.lisp \
    2>> logs/clisp-fresh.log >> logs/clisp-fresh.log
grep TOTAL logs/clisp-fresh.log | tail -1

echo ------------------------------------------------------------
echo Testing the trunk \(without recompiling\) on CLISP
echo > logs/clisp-fresh.log
clisp -i inputs/clisp/init.lisp inputs/clisp/nst-tests.lisp \
    2>> logs/clisp-fresh.log >> logs/clisp-fresh.log
grep TOTAL logs/clisp-fresh.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on CLISP
mkdir -p clisp-junit
rm -f clisp-junit/*
export NSTJUNITDIR=clisp-junit/
clisp -i inputs/clisp/init.lisp inputs/clisp/junit.lisp \
    2>> clisp-junit/junit.log >> clisp-junit/junit.log
echo Files in $NSTJUNITDIR directory: expected 4, have \
    `ls -1 $NSTJUNITDIR | wc -l`



# Clear out Clozure FASLs
rm -rf /home/jm/Lib/Lisp/fasl/misc/ccl-*/*

echo ------------------------------------------------------------
echo Testing the trunk \(with everything recompiled\) on Clozure
echo > logs/clozure-fresh.log
~/Lib/Lisp/clozure/ccl/lx86cl --batch < inputs/clozure/nst-tests.lisp \
    2>> logs/clozure-fresh.log >> logs/clozure-fresh.log
grep TOTAL logs/clozure-fresh.log | tail -1

echo ------------------------------------------------------------
echo Testing the trunk \(without recompiling\) on Clozure
echo > logs/clozure-reload.log
~/Lib/Lisp/clozure/ccl/lx86cl --batch < inputs/clozure/nst-tests.lisp \
    2>> logs/clozure-reload.log >> logs/clozure-reload.log
grep TOTAL logs/clozure-reload.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Clozure
mkdir -p clozure-junit
rm -f clozure-junit/*
export NSTJUNITDIR=clozure-junit/
~/Lib/Lisp/clozure/ccl/lx86cl --batch < inputs/clozure/junit.lisp \
    2>> clozure-junit/junit.log >> clozure-junit/junit.log
echo Files in $NSTJUNITDIR directory: expected 4, have \
    `ls -1 $NSTJUNITDIR | wc -l`


# Clear out SBCL FASLs
rm -rf /home/jm/Lib/Lisp/fasl/sbcl/*/*

echo ------------------------------------------------------------
echo Testing the trunk \(with everything recompiled\) on SBCL
echo > logs/sbcl-fresh.log
sbcl < inputs/sbcl/nst-tests.lisp \
    2>> logs/sbcl-fresh.log >> logs/sbcl-fresh.log
grep TOTAL logs/sbcl-fresh.log | tail -1

echo ------------------------------------------------------------
echo Testing the trunk \(without recompiling\) on SBCL
echo > logs/sbcl-reload.log
sbcl < inputs/sbcl/nst-tests.lisp \
    2>> logs/sbcl-reload.log >> logs/sbcl-reload.log
grep TOTAL logs/sbcl-reload.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on SBCL
mkdir -p sbcl-junit
rm -f sbcl-junit/*
export NSTJUNITDIR=sbcl-junit/
sbcl < inputs/sbcl/junit.lisp \
    2>> sbcl-junit/junit.log >> sbcl-junit/junit.log
echo Files in $NSTJUNITDIR directory: expected 4, have \
    `ls -1 $NSTJUNITDIR | wc -l`


 
# echo Testing the trunk on CMU CL
# echo > logs/cmucl-fresh.log
# cmucl -batch < trunk-asdfind.lisp 2>> logs/cmucl-fresh.log >> logs/cmucl-fresh.log
# grep TOTAL logs/cmucl-fresh.log | tail -1



# Clear out Allegro FASLs
rm -rf /home/jm/Lib/Lisp/fasl/allegro/*/*

echo ------------------------------------------------------------
echo Testing the trunk \(with everything recompiled\) on Allegro 8.2 mlisp
echo > logs/allegro82-mlisp-fresh.log
mlisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro82-mlisp-fresh.log >> logs/allegro82-mlisp-fresh.log
grep TOTAL logs/allegro82-mlisp-fresh.log | tail -1

echo ------------------------------------------------------------
echo Testing the trunk \(without recompiling\) on Allegro 8.2 mlisp
echo > logs/allegro82-mlisp-reload.log
mlisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro82-mlisp-reload.log >> logs/allegro82-mlisp-reload.log
grep TOTAL logs/allegro82-mlisp-reload.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Allegro 8.2 mlisp
mkdir -p acl82-mlisp-junit
rm -f acl82-mlisp-junit/*
export NSTJUNITDIR=acl82-mlisp-junit/
mlisp < inputs/acl/junit.lisp \
    2>> acl82-mlisp-junit/junit.log >> acl82-mlisp-junit/junit.log
echo Files in $NSTJUNITDIR directory: expected 4, have \
    `ls -1 $NSTJUNITDIR | wc -l`



echo ------------------------------------------------------------
echo Testing the trunk \(with everything recompiled\) on Allegro 8.2 alisp
echo > logs/allegro82-alisp-fresh.log
alisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro82-alisp-fresh.log >> logs/allegro82-alisp-fresh.log
grep TOTAL logs/allegro82-alisp-fresh.log | tail -1

echo ------------------------------------------------------------
echo Testing the trunk \(without recompiling\) on Allegro 8.2 alisp
echo > logs/allegro82-alisp-reload.log
alisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro82-alisp-reload.log >> logs/allegro82-alisp-reload.log
grep TOTAL logs/allegro82-alisp-reload.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Allegro 8.2 alisp
mkdir -p acl82-alisp-junit
rm -f acl82-alisp-junit/*
export NSTJUNITDIR=acl82-alisp-junit/
alisp < inputs/acl/junit.lisp \
    2>> acl82-alisp-junit/junit.log >> acl82-alisp-junit/junit.log
echo Files in $NSTJUNITDIR directory: expected 4, have \
    `ls -1 $NSTJUNITDIR | wc -l`



echo ------------------------------------------------------------
echo Testing the trunk \(with everything recompiled\) on Allegro 8.1 mlisp
echo > logs/allegro81-mlisp-fresh.log
mlisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro81-mlisp-fresh.log >> logs/allegro81-mlisp-fresh.log
grep TOTAL logs/allegro81-mlisp-fresh.log | tail -1

echo ------------------------------------------------------------
echo Testing the trunk \(without recompiling\) on Allegro 8.1 mlisp
echo > logs/allegro81-mlisp-reload.log
mlisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro81-mlisp-reload.log >> logs/allegro81-mlisp-reload.log
grep TOTAL logs/allegro81-mlisp-reload.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Allegro 8.1 mlisp
mkdir -p acl81-mlisp-junit
rm -f acl81-mlisp-junit/*
export NSTJUNITDIR=acl81-mlisp-junit/
/usr/local/allegro/acl81/mlisp < inputs/acl/junit.lisp \
    2>> acl81-mlisp-junit/junit.log >> acl81-mlisp-junit/junit.log
echo Files in $NSTJUNITDIR directory: expected 4, have \
    `ls -1 $NSTJUNITDIR | wc -l`



echo ------------------------------------------------------------
echo Testing the trunk \(with everything recompiled\) on Allegro 8.1 alisp
echo > logs/allegro81-alisp-fresh.log
alisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro81-alisp-fresh.log >> logs/allegro81-alisp-fresh.log
grep TOTAL logs/allegro81-alisp-fresh.log | tail -1

echo ------------------------------------------------------------
echo Testing the trunk \(without recompiling\) on Allegro 8.1 alisp
echo > logs/allegro81-alisp-reload.log
alisp < inputs/acl/nst-tests.lisp \
    2>> logs/allegro81-alisp-reload.log >> logs/allegro81-alisp-reload.log
grep TOTAL logs/allegro81-alisp-reload.log | tail -1

echo ------------------------------------------------------------
echo Testing JUnit writing on Allegro 8.1 alisp
mkdir -p acl81-alisp-junit
rm -f acl81-alisp-junit/*
export NSTJUNITDIR=acl81-alisp-junit/
/usr/local/allegro/acl81/alisp < inputs/acl/junit.lisp \
    2>> acl81-alisp-junit/junit.log >> acl81-alisp-junit/junit.log
echo Files in $NSTJUNITDIR directory: expected 4, have \
    `ls -1 $NSTJUNITDIR | wc -l`
