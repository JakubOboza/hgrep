#!/usr/bin/env zsh
set -x
ghc -o bin/hgrep src/Hrgrep.hs
rm src/*.o
rm src/*.hi
