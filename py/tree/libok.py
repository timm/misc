#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

from lib import *

print(cli())
for l in file("lib.py"):
  print("["+l+"]")
