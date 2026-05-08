#!/usr/bin/env bash
# lisp2html.sh TITLE < file.lisp > file.html
# Pipeline: lisp -> md (via lisp2md.sh) -> html (via pandoc).
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"
TITLE="${1:-Story}"

bash "$HERE/lisp2md.sh" "$TITLE" | pandoc \
  --standalone \
  --metadata title="$TITLE" \
  --highlight-style=tango \
  --css=https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11/build/styles/github.min.css \
  -H <(cat <<'EOF'
<style>
body { max-width: 720px; margin: 2em auto; padding: 0 1em;
       font-family: Georgia, serif; font-size: 17px; line-height: 1.55;
       color: #222; background: #fdfdfd; }
h1, h2, h3 { font-family: Helvetica, sans-serif; }
h1 { border-bottom: 1px solid #ccc; padding-top: 1em; padding-bottom: .2em; }
h2 { padding-top: .8em; }
blockquote { border-left: 3px solid #ccc; color: #555;
             margin-left: 0; padding-left: 1em; }
pre { background: #f6f6f6; padding: .6em 1em;
      border-left: 3px solid #ddd; overflow-x: auto;
      font-size: 14px; line-height: 1.4; }
code { font-family: Menlo, monospace; font-size: 90%; }
table { border-collapse: collapse; margin: 1em 0; }
th, td { border-bottom: 1px dotted #888; padding: 4px 10px;
         text-align: left; font-size: 14px; }
hr { border: 0; border-top: 1px dotted #888; margin: 2em 0; }
</style>
EOF
)
