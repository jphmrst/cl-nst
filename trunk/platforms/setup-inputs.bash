#!/bin/bash

rm -rf inputs
mkdir inputs

mkdir inputs/clisp
cat src/load-binloc-via-asdf.lisp src/clisp-init.lisp \
    > inputs/clisp/init.lisp
cat src/path-init-by-push.lisp src/run-nst-tests.lisp \
    > inputs/clisp/nst-tests.lisp
cat src/path-init-by-push.lisp src/run-junit.lisp \
    > inputs/clisp/junit.lisp

mkdir inputs/clozure
cat src/path-init-by-push.lisp src/run-nst-tests.lisp \
    > inputs/clozure/nst-tests.lisp
cat src/path-init-by-push.lisp src/run-junit.lisp \
    > inputs/clozure/junit.lisp

mkdir inputs/sbcl
cat src/path-init-by-asdfind.lisp src/run-nst-tests.lisp \
    > inputs/sbcl/nst-tests.lisp
cat src/path-init-by-asdfind.lisp src/run-junit.lisp \
    > inputs/sbcl/junit.lisp

mkdir inputs/acl
cat src/path-init-by-asdfind.lisp src/run-nst-tests.lisp \
    > inputs/acl/nst-tests.lisp
cat src/path-init-by-asdfind.lisp src/run-junit.lisp \
    > inputs/acl/junit.lisp

mkdir inputs/lispworks
cat src/load-asdf.lisp src/path-init-by-push.lisp src/run-nst-tests.lisp \
    > inputs/lispworks/nst-tests.lisp
cat src/load-asdf.lisp src/path-init-by-push.lisp src/run-junit.lisp \
    > inputs/lispworks/junit.lisp

mkdir inputs/scieneer
cat src/path-init-by-push.lisp src/run-nst-tests.lisp src/explicit-quit.lisp \
    > inputs/scieneer/nst-tests.lisp
cat src/path-init-by-push.lisp src/run-junit.lisp src/explicit-quit.lisp \
    > inputs/scieneer/junit.lisp

