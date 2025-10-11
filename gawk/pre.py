#!/usr/bin/env python3 -B
import sys

head=None
with open(sys.argv[1], 'r') as f:
  for line in f:
    line = line.strip().split(",")
    if not head:
      head=line
      for i,s in enumerate(line):
        if s[0].isupper():
          sd[i]=mu[i]=m2[i]=n[i] =0
     
print(sd)
