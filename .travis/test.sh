#!/usr/bin/env bash

source .travis/setup.sh

$stack build --ghc-options=-Werror

for dir in *; do
  if [[ -d "$dir" ]] && [[ -d "$dir/server" ]]; then
    $stack exec -- "$dir" &
    sleep 1 # wait for server to come up
    $stack exec -- "$dir-test"
  fi
done
