#!/usr/bin/sh

find "$1" -type f -printf '%p\n' | xargs sha1sum

