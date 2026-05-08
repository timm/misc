# prep:  .f after value-char  ->  ["f"]      (field access)
#        .x bare              ->  HEAP[x]    (object reference)
{ s = gensub(/([A-Za-z0-9_\]\)])\.([A-Za-z_][A-Za-z_0-9]*)/, \
             "\\1[\"\\2\"]", "g")
  print gensub(/\.([A-Za-z_][A-Za-z_0-9]*)/, "HEAP[\\1]", "g", s) }
