#!/usr/bin/awk -f
# Reverse index. For every `### NAME` heading, scan later lines for
# mentions of NAME in prose or code. Emit a final appendix table.
{ all[NR] = $0; if (match($0, /^### /)) {
    h = substr($0, 5); gsub(/^[ \t]+|[ \t]+$/, "", h)
    # Heading might be `### 4.1 Sym`; take last whitespace-separated token.
    nf = split(h, a, /[ \t]+/); name = a[nf]
    if (length(name) > 1 && !(name in defined)) {
      defined[name] = NR; order[++ndef] = name
    }
  }
}
END {
  for (i = 1; i <= ndef; i++) {
    name = order[i]; def_line = defined[name]; used = ""
    for (j = def_line + 1; j <= NR; j++) {
      if (index(all[j], name)) {
        if (match(all[j], /^### /)) {
          h = substr(all[j], 5); gsub(/^[ \t]+|[ \t]+$/, "", h)
          used = used (used ? ", " : "") h
        }
      }
    }
    if (used != "") usedin[name] = used
  }
  for (i = 1; i <= NR; i++) print all[i]
  print ""
  print "## Appendix Z. Used-in index"
  print ""
  print "Auto-generated. Each name links to its definition; the list is"
  print "where it is referenced afterward."
  print ""
  for (i = 1; i <= ndef; i++) {
    name = order[i]
    if (name in usedin) printf("- `%s` — used in: %s\n", name, usedin[name])
  }
}
