#!/bin/bash

for m in `find . -name \\*.lisp -print`
do a2ps -2r -o `echo $m | sed 's/.lisp$/.ps/'` $m
done

for m in `find . -name \\*.asd -print`
do a2ps -2r -o `echo $m | sed 's/.asd$/.ps/'` $m
done
