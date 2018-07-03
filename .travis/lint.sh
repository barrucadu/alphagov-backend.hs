#!/usr/bin/env bash

source .travis/setup.sh

$stack build

curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .

$stack install brittany stylish-haskell

find . -name '*.hs' -exec brittany --write-mode inplace {} \;
find . -name '*.hs' -exec stylish-haskell -i {} \;
git diff --exit-code
