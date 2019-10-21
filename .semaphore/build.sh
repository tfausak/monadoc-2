#! /usr/bin/env sh
set -o errexit -o xtrace

mkdir --parents .stack function/lib

stack build \
  --copy-bins \
  --local-bin-path function

ldd function/monadoc |
  awk '/=>/ { print $3 }' |
  xargs cp \
    --target-directory function/lib \
    --verbose

( cd function && zip -r ../function.zip . )
