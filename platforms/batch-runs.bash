#!/bin/bash
mkdir logs
cat < /dev/null > logs/cmucl0.log
cmucl -batch < cmucl.lisp 2>> logs/cmucl0.log >> logs/cmucl0.log
