#!/bin/bash

# git ls-files | entr -prc raco test --drdr --quiet-program --heartbeat --make -p mahjong
# git ls-files | entr -prc raco test --drdr --quiet-program --heartbeat --make "main.rkt"
git ls-files | entr -prc raco test --drdr --quiet-program --heartbeat --make "main.rkt"
