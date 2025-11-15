#!/usr/bin/env gawk -f
BEGIN   { FS=","
          RAW=0
          BEST=1
          REST=2}
        { gsub(/[ \t]+,"") }
NR==1   { for(c=1;c<=NF;c++) {
            names[c] = $c
            if($c ~ /^[A-Z]/)  is_num[c]
            if($c !~ /[+-!]$/) x[c]
            if($c  ~ /-$/)     y[c]=0
            if($c  ~ /+$/)     y[c]=1 }
          next}
NR > 1  { for(c in names) row[0][NF-1][c] = set(RAW,NF-1,c,$c) }
END     { for(c in num) asort(num[RAW][c]) 
          threshold = _threshold()
          for(r in row) {
            klass = dist(row[r]) <= threshold ? BEST : REST 
            fprbad[r] : good[r] }}

function set(klass,r,c,v) {
  row[klass][r][c] = v   
  if (c in is_num) 
    if (v != "?") {
      v += 0
      num[klass][c][r] = v }
  return v }

function _threshold(     tmp,r) {
  for(r in row) tmp[r] = disty(row[r])
  asort(tmp)
  return tmp[ int(sqrt(length(tmp))) ] }

function disty(row,    c,d,n) {
  for(c in y) { n++; d += (norm(c,row[c]) - y[c])^2 }
  return sqrt(d/(n+1e-32)) }

function norm(c,n,    hi) { 
  hi = num[length(num)]
  return (n - num[1]) / (hi - num[1] + 1e-32) }
