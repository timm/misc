#!/usr/bin/env python3
"""
postfix.py: patch pycco html. Adds right-side "Fork me on GitHub" banner
and sets paragraph text alignment.

Usage: postfix.py HTML REPO_URL ALIGN
"""
import sys

RIBBON = ("https://github.blog/wp-content/uploads/2008/12/"
          "forkme_right_darkblue_121621.png?resize=149%2C149")

def banner(repo):
  return (f'<a href="{repo}"><img width="149" height="149" '
          'style="position:absolute;top:0;right:0;border:0;z-index:99;" '
          f'src="{RIBBON}" alt="Fork me on GitHub"></a>')

def style(align):
  return f"<style>.docs p, .docs li {{ text-align: {align}; }}</style>"

if __name__ == "__main__":
  path, repo, align = sys.argv[1:4]
  with open(path, encoding="utf-8") as f: h = f.read()
  h = h.replace("</head>", style(align) + "</head>", 1)
  h = h.replace("<body>", "<body>" + banner(repo), 1)
  with open(path, "w", encoding="utf-8") as f: f.write(h)
  print(f"postfix: {path} (banner -> {repo}, align={align})")
