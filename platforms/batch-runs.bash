#!/bin/bash
mkdir -p logs

# echo Testing the branch on ECL
# echo > logs/ecl-branch.log
# ecl -load branch-pushes.lisp 2>> logs/ecl-branch.log >> logs/ecl-branch.log
# grep TOTAL logs/ecl-branch.log | tail -1

# echo Testing the trunk on ECL
# echo > logs/ecl-trunk.log
# ecl -load trunk-pushes.lisp 2>> logs/ecl-trunk.log >> logs/ecl-trunk.log
# grep TOTAL logs/ecl-trunk.log | tail -1

echo Testing the branch on CLISP
echo > logs/clisp-branch.log
clisp -i clisp-init.lisp branch-pushes.lisp 2>> logs/clisp-branch.log >> logs/clisp-branch.log
grep TOTAL logs/clisp-branch.log | tail -1

echo Testing the trunk on CLISP
echo > logs/clisp-trunk.log
clisp -i clisp-init.lisp trunk-pushes.lisp 2>> logs/clisp-trunk.log >> logs/clisp-trunk.log
grep TOTAL logs/clisp-trunk.log | tail -1

echo Testing the branch on Clozure
echo > logs/clozure-branch.log
~/Lib/Lisp/clozure/ccl/lx86cl --batch < branch-pushes.lisp 2>> logs/clozure-branch.log >> logs/clozure-branch.log
grep TOTAL logs/clozure-branch.log | tail -1

echo Testing the trunk on Clozure
echo > logs/clozure-trunk.log
~/Lib/Lisp/clozure/ccl/lx86cl --batch < trunk-pushes.lisp 2>> logs/clozure-trunk.log >> logs/clozure-trunk.log
grep TOTAL logs/clozure-trunk.log | tail -1

echo Testing the branch on SBCL
echo > logs/sbcl-branch.log
sbcl < branch-asdfind.lisp 2>> logs/sbcl-branch.log >> logs/sbcl-branch.log
grep TOTAL logs/sbcl-branch.log | tail -1

echo Testing the trunk on SBCL
echo > logs/sbcl-trunk.log
sbcl < trunk-asdfind.lisp 2>> logs/sbcl-trunk.log >> logs/sbcl-trunk.log
grep TOTAL logs/sbcl-trunk.log | tail -1

# echo Testing the branch on CMU CL
# echo > logs/cmucl-branch.log
# cmucl -batch < branch-asdfind.lisp 2>> logs/cmucl-branch.log >> logs/cmucl-branch.log
# grep TOTAL logs/cmucl-branch.log | tail -1
# 
# echo Testing the trunk on CMU CL
# echo > logs/cmucl-trunk.log
# cmucl -batch < trunk-asdfind.lisp 2>> logs/cmucl-trunk.log >> logs/cmucl-trunk.log
# grep TOTAL logs/cmucl-trunk.log | tail -1
