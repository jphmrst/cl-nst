#!/bin/bash

rm -rf inputs
mkdir inputs
mkdir inputs/clisp
mkdir inputs/acl
mkdir inputs/sbcl
mkdir inputs/clozure
mkdir inputs/lispworks

cat src/load-binloc-via-asdf.lisp src/clisp-init.lisp \
    > inputs/clisp/init.lisp
cat src/path-init-by-push.lisp src/run-nst-tests.lisp \
    > inputs/clisp/nst-tests.lisp
cat src/path-init-by-push.lisp src/run-junit.lisp \
    > inputs/clisp/junit.lisp

cat src/path-init-by-push.lisp src/run-nst-tests.lisp \
    > inputs/clozure/nst-tests.lisp
cat src/path-init-by-push.lisp src/run-junit.lisp \
    > inputs/clozure/junit.lisp

cat src/path-init-by-asdfind.lisp src/run-nst-tests.lisp \
    > inputs/sbcl/nst-tests.lisp
cat src/path-init-by-asdfind.lisp src/run-junit.lisp \
    > inputs/sbcl/junit.lisp

cat src/path-init-by-asdfind.lisp src/run-nst-tests.lisp \
    > inputs/acl/nst-tests.lisp
cat src/path-init-by-asdfind.lisp src/run-junit.lisp \
    > inputs/acl/junit.lisp

cat src/load-asdf.lisp src/path-init-by-push.lisp src/run-nst-tests.lisp \
    > inputs/lispworks/nst-tests.lisp
cat src/load-asdf.lisp src/path-init-by-push.lisp src/run-junit.lisp \
    > inputs/lispworks/junit.lisp

