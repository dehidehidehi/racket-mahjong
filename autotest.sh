#!/bin/bash

git ls-files | entr -rc raco test --drdr --quiet-program --heartbeat --make -p mahjong
