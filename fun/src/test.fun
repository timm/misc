# vim: nospell ts=2 sw=2 sts=2  et :

function rogues(    s) {
  for(s in SYMTAB) {
     if (s ~ /^[A-Z][a-z]/) print "Global " s
     if (s ~ /^[_a-z]/    ) print "Rogue: " s }
}

function tests(what, all,   one,a,i,n) {
  n = split(all,a,",")
  print "\n#--- " what " -----------------------"
  for(i=1;i<=n;i++) { 
    one = a[i]; @one(one) }
  rogues()
}

function is(f,got,want,   epsilon,     ok) {
  if (typeof(want) == "number") {
     epsilon = epsilon ? epsilon : 0.001
     ok = abs(want - got)/(want + 10^-32)  < epsilon
  } else
     ok = want == got
  if (ok) 
    print "#TEST:\tPASSED\t" f "\t" want "\t" got 
  else 
    print "#TEST:\tFAILED\t" f "\t" want "\t" got 
}
