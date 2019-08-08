#!/usr/bin/env sh

# SBCL
sbcl --noinform --non-interactive --load boot.lisp

exit $?

# CCL 64 bits
wx86cl64 -l boot.lisp

# CCL 32 bits
wx86cl -l boot.lisp

