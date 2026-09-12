#!/usr/bin/env python3
"""
pyccoize.py: rewrite a python file for nicer pycco output.

For every line of the form

    def name(args): # comment

emit instead

    # `def name(args)`
    #
    # comment
    def name(args):

Usage: pyccoize.py IN OUT
"""
import re, sys

DEF = re.compile(r"^(\s*)(def\s+\w+\s*\(.*?\)\s*(?:->\s*[^:]+)?)\s*:\s*#\s*(.*?)\s*$")

def pyccoize(src):
  out = []
  for line in src.splitlines():
    m = DEF.match(line)
    if m:
      indent, sig, comment = m.groups()
      out += [f"{indent}# `{sig}`", f"{indent}#", f"{indent}# {comment}", f"{indent}{sig}:"]
    else:
      out.append(line)
  return "\n".join(out) + "\n"

if __name__ == "__main__":
  src, dst = sys.argv[1], sys.argv[2]
  with open(src, encoding="utf-8") as f: text = f.read()
  with open(dst, "w", encoding="utf-8") as f: f.write(pyccoize(text))
  n = sum(1 for l in text.splitlines() if DEF.match(l))
  print(f"pyccoize: {src} -> {dst} ({n} def-comments lifted)")
