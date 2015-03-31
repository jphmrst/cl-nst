#!/bin/bash

fgrep -e "$1" */*.lisp */*/*.lisp */*/*/*.lisp */*/*/*/*.lisp \
    *.asd */*.asd */*/*.asd
