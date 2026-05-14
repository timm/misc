#!/usr/bin/awk -f
# Collect all `### Heading` slugs. Then scan `[name](#anchor)` links.
# Fail if any anchor has no matching slug.
function slugify(s,   t) {
  t = tolower(s); gsub(/[^a-z0-9 -]/, "", t); gsub(/ +/, "-", t); return t
}
{ all[NR] = $0; if (match($0, /^### /)) {
    h = substr($0, 5); gsub(/^[ \t]+|[ \t]+$/, "", h)
    slugs[slugify(h)] = 1
  }
}
END {
  bad = 0
  for (i = 1; i <= NR; i++) {
    line = all[i]
    while (match(line, /\]\(#[A-Za-z0-9_-]+\)/)) {
      tok = substr(line, RSTART + 3, RLENGTH - 4)
      if (!(tok in slugs)) {
        printf("xref-check: line %d: broken anchor #%s\n", i, tok) > "/dev/stderr"
        bad++
      }
      line = substr(line, RSTART + RLENGTH)
    }
  }
  if (bad > 0) exit 1
}
