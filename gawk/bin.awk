#!/usr/bin/env gawk -f
BEGIN { FS=","; BINS=7; CONVFMT = "%.2f" }
      { gsub(/[ \t\r]/,"") }
NR==1 { for(i=1;i<=NF;i++) 
          if (($i ~ /^[A-Z]/) && ($i !~ /[-+!]$/))  {
            hi[i] = -(lo[i] = 1E32)
            for(b=1; b<BINS; b++)
              bmin[i][b] = 1E32 }}
NR>1  { for(i=1;i<=NF;i++) 
          row[NR-1][i] = seen(i,$i) }
END   { for(r in row) 
          for(i=1;i<=NF;i++) bin(i,row[r][i])
        for(r in row) {
          s=sep=""
          for(i=1;i<=NF;i++) {
            s = s sep bin(i, row[r][i]); sep = "," }
          print s }}

function seen(i,v,    d) {         
  if ((v!="?") && (i in hi)) {
    v     += 0
    n[i]  += 1
    d      = v - mu[i]
    mu[i] += d/++n[i]
    m2[i] += d*(v-mu[i]) 
    sd[i]  = n[i] < 2 ? 0 : (m2[i]/(n[i]-1))^0.5 }
  return v }

function bin(i,v,    b) {
  if ((v!="?") && (i in hi)) {
    b = int(BINS / (1 + exp(-1.704 * (v - mu[i]) / (sd[i] + 1E-32))))
    b = b < 1 ? 1 : b >= BINS ? BINS - 1 : b
    if (v < bmin[i][b]) bmin[i][b] = v
    v = bmin[i][b] }
  return v }


# not working  wrong 0

