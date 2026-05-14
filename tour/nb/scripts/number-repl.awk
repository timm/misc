#!/usr/bin/awk -f
# Two passes. First: scan for `[?]>`/`[?T]>` -> assign global numbers.
# Track `<!-- ref:LABEL -->` comments to map labels -> their prompt N.
# Second pass: rewrite each `[?]>` -> `[N]>`, `[?T]>` -> `[TN]>`,
# and rewrite `[ref:LABEL]` in prose to the assigned N.
{ lines[NR] = $0 }

END {
  n = 0; tn = 0
  pending_label = ""
  for (i = 1; i <= NR; i++) {
    line = lines[i]
    if (match(line, /<!--[ \t]*ref:[A-Za-z0-9_-]+[ \t]*-->/)) {
      lab = substr(line, RSTART, RLENGTH)
      gsub(/<!--[ \t]*ref:/, "", lab); gsub(/[ \t]*-->/, "", lab)
      pending_label = lab
      continue
    }
    if (line ~ /\[\?T\]>/) {
      tn++
      gsub(/\[\?T\]>/, "[T" tn "]>", line)
      if (pending_label != "") { ref["T:" pending_label] = tn; pending_label = "" }
      lines[i] = line
    } else if (line ~ /\[\?\]>/) {
      n++
      gsub(/\[\?\]>/, "[" n "]>", line)
      if (pending_label != "") { ref[pending_label] = n; pending_label = "" }
      lines[i] = line
    }
  }
  # Pass 2: rewrite [ref:LABEL] tokens in any line.
  for (i = 1; i <= NR; i++) {
    line = lines[i]
    while (match(line, /\[ref:[A-Za-z0-9_-]+\]/)) {
      tok = substr(line, RSTART, RLENGTH)
      lab = substr(tok, 6, length(tok) - 6)
      val = (lab in ref) ? ref[lab] : (("T:" lab) in ref ? "T" ref["T:" lab] : "?")
      line = substr(line, 1, RSTART - 1) val substr(line, RSTART + RLENGTH)
    }
    print line
  }
}
