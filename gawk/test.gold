#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

function rogues(    s) {
  for(s in SYMTAB) 
    if (s ~ /^[A-Z][a-z]/) print "Global " s
  for(s in SYMTAB) 
    if (s ~ /^[_a-z]/) print "Rogue: " s
}

function tests(what, all,   one,a,i,n) {
  n = split(all,a,",")
  print "\n#--- " what " -----------------------"
  for(i=1;i<=n;i++) { 
    one = a[i]; @one(one) }
  rogues()
}

BEGIN { AU.test.epsilon = 0.0001 }

function ok(f,got,want,   epsilon,     good) {
  epsilon = epsilon ? epsilon : AU.test.epsilon
  if (typeof(want) == "number") 
    good = abs(want - got)/(want + 10^-32)  < epsilon
  else
    good = want == got;
  print "#TEST:\t"(good?"PASSED":"FAILED") "\t" f "\t" want "\t" got 
}

