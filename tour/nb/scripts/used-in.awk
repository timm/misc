#!/usr/bin/awk -f
# Reverse index.
# Pass 1: collect every `### Heading`; the LAST whitespace-token of
# the heading is the "name". Track which line ranges belong to each
# section heading.
# Pass 2: for each defined name, scan every section OTHER than its
# own definition section. If the section contains the name in any
# line, record "name -> that section heading".
{ all[NR] = $0
  if (match($0, /^### /)) {
    h = substr($0, 5); gsub(/^[ \t]+|[ \t]+$/, "", h)
    nf = split(h, a, /[ \t]+/); name = a[nf]
    gsub(/[`*_]/, "", name)
    # Section bookkeeping for every heading.
    nsec++
    sec_name[nsec] = name
    sec_head[nsec] = h
    sec_line[nsec] = NR
    # But only stanzas named after identifiers (not Lecture/Breakout
    # headings or appendix prose) qualify as a defined name.
    if (h !~ /^Breakout/ && h !~ /^Lecture/ && h !~ /^Appendix/ \
        && h !~ /^Tables/ && h !~ /^Multiple/ && h !~ /missing/) {
      if (!(name in def_idx)) def_idx[name] = nsec
      def_list[++nd] = name
    }
  }
}
END {
  sec_line[nsec + 1] = NR + 1
  for (x = 1; x <= nd; x++) {
    name = def_list[x]; own = def_idx[name]
    seen_here = ""
    for (j = own + 1; j <= nsec; j++) {
      start = sec_line[j] + 1; finish = sec_line[j + 1] - 1
      hit = 0
      for (k = start; k <= finish && !hit; k++)
        if (index(all[k], name)) hit = 1
      if (hit) seen_here = seen_here (seen_here ? ", " : "") sec_head[j]
    }
    if (seen_here != "") usedin[name] = seen_here
  }
  for (i = 1; i <= NR; i++) print all[i]
  print ""
  print "## Appendix Z. Used-in index"
  print ""
  print "Auto-generated. For each defined name, lists every later"
  print "section that mentions it."
  print ""
  for (x = 1; x <= nd; x++) {
    name = def_list[x]
    if (name in usedin) printf("- `%s` — used in: %s\n", name, usedin[name])
  }
}
