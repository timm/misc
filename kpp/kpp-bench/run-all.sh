#!/usr/bin/env bash

echo "Running JavaScript..."
/usr/bin/time ./kpp.js

echo "Running Lua..."
/usr/bin/time lua kpp.lua

echo "Running Gawk..."
/usr/bin/time ./kpp.awk big.csv

echo "Running python..."
/usr/bin/time ./kpp.py
