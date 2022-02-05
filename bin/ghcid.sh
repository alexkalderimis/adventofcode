#!/bin/bash

TEST="${3:-test}"

# Run GHCID in this project
ghcid --command="./bin/repl $1 $2" \
      --reload="$1/$2/Main.hs" \
      --test="hspec $TEST" \
      --max-messages=5 \
      --ignore-loaded \
      --color=always
