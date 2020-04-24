#!/bin/sh -e

EMACS="${EMACS:=emacs}"

$EMACS -batch -l native-complete.el -l native-complete-test.el -f ert-run-tests-batch-and-exit
