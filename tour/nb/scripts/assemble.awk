#!/usr/bin/awk -f
# Inline include directives.
#   <!-- include chunks/NN_slug.md --> -> contents of that file.
/<!--[ \t]*include[ \t]+[^ ]+[ \t]*-->/ {
  match($0, /include[ \t]+[^ ]+/)
  s = substr($0, RSTART+7, RLENGTH-7)
  gsub(/^[ \t]+|[ \t]+$/, "", s)
  while ((getline line < s) > 0) print line
  close(s)
  next
}
{ print }
