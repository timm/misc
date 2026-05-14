#!/usr/bin/awk -f
# Pass 1 collects ### headings into a TOC.
# Pass 2 (after first `# Title`) injects a "**Contents**" bar.
{ all[NR] = $0; if (/^### /) hdrs[++nh] = $0 }
END {
  injected = 0
  for (i = 1; i <= NR; i++) {
    print all[i]
    if (!injected && all[i] ~ /^# /) {
      print ""
      print "**Contents**"
      print ""
      for (j = 1; j <= nh; j++) {
        h = hdrs[j]; sub(/^### /, "", h)
        slug = tolower(h); gsub(/[^a-z0-9 -]/, "", slug); gsub(/ +/, "-", slug)
        print "- [" h "](#" slug ")"
      }
      print ""
      injected = 1
    }
  }
}
