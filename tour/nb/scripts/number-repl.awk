#!/usr/bin/awk -f
# Pass 1: assign global numbers to `[?]>` and `[?T]>`.
# `<!-- ref:LABEL -->` queues a label; every queued label resolves
# to the NEXT prompt seen. Multiple queued labels all bind to the
# same next prompt (so `l1-end` and `l2-start` can sit next to each
# other and resolve to the same number).
# Pass 2: rewrite `[ref:LABEL]` tokens to the assigned number.
{ lines[NR] = $0 }

END {
  n = 0; tn = 0
  pq = 0; tpq = 0
  for (i = 1; i <= NR; i++) {
    line = lines[i]
    if (match(line, /<!--[ \t]*ref:[A-Za-z0-9_-]+[ \t]*-->/)) {
      lab = substr(line, RSTART, RLENGTH)
      gsub(/<!--[ \t]*ref:/, "", lab); gsub(/[ \t]*-->/, "", lab)
      pending[++pq] = lab
      continue
    }
    if (line ~ /\[\?T\]>/) {
      tn++
      gsub(/\[\?T\]>/, "[T" tn "]>", line)
      for (k = 1; k <= pq; k++) ref["T:" pending[k]] = tn
      pq = 0
      lines[i] = line
    } else if (line ~ /\[\?\]>/) {
      n++
      gsub(/\[\?\]>/, "[" n "]>", line)
      for (k = 1; k <= pq; k++) ref[pending[k]] = n
      pq = 0
      lines[i] = line
    }
  }
  # Pass 2: substitute [ref:LABEL] tokens.
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
