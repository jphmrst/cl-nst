#!/bin/bash
mkdir logs

echo > logs/cmucl1.log
cmucl -batch < cmucl1.lisp 2>> logs/cmucl1.log >> logs/cmucl1.log

echo > logs/cmucl0.log
cmucl -batch < cmucl0.lisp 2>> logs/cmucl0.log >> logs/cmucl0.log

