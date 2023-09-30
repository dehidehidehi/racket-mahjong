#!/bin/bash

git ls-files | entr -prc raco test --drdr --quiet-program --heartbeat --make -p mahjong
