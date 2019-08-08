#!/usr/bin/env sh


sbcl --noinform --eval "(ql:quickload '(swank looset))" --eval '(swank:create-server)'

