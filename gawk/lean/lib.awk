function trim(s) { sub(/^[ \t]*/,"",s); sub(/[ \t*]$/,"",s); return s}

function per(a,p)  { return a[int(p*length(a))] }
function median(a) { return per(a,.5)    }
function sd(a)     { return (per(a,.9) - per(a,.1)) / 2.56 }

function mode(a,    most,out) {
  for (i in a) if (a[i] > most) {most=a[i]; out=i}
  return out }

function ent(a,   N,e) {
  for(i in a) N += a[i]
  for(i in a) e  = e - a[i]/N * log(a[i]/N)/log(2)
  return e }

-- ## Classifier performance stats -------------------------------------

function l.ABCD(klass, b4)
  return {klass=klass, a=(b4 or 0), b=0, c=0, d=0} end

function l.abcd(abcd1, want,got)
  if   want == abcd1.klass
  then if want==got       then abcd1.d=abcd1.d+1 else abcd1.b=abcd1.b+1 end
  else if got==abcd1.klass then abcd1.c=abcd1.c+1 else abcd1.a=abcd1.a+1 end end end

function l.pf(abcd1)        return abcd1.c           / (abcd1.a+abcd1.c+1E-30) end
function l.recall(abcd1)    return abcd1.d           / (abcd1.b+abcd1.d+1E-30) end
function l.accuracy(abcd1)  return (abcd1.a+abcd1.d) / (abcd1.a+abcd1.b+abcd1.c+abcd1.d+1E-30) end
function l.precision(abcd1) return abcd1.d           / (abcd1.c+abcd1.d+1E-30) end
function l.f(abcd1,   pr)   p,r  = l.precision(abcd1),l.recall(abcd1); return (2*p*r)  / (p+r) end
function l.g(abcd1,   nf)   nf,r = 1-l.pf(abcd),l.recall(abcd);        return (2*nf*r) / (nf+r) end

function l.ABCDS() {return {all={}, n=0} end

function l.abcds(abcds,want,got)
  abcds.all[want] = abcds.all[want] or l.ABCD(want, abcds.n)
  abcds.n         = abcds.n + 1
  for _,abcd1 in pairs(abcds.all) do add(abcd1,want,got) end end

function l.abcdsreport(abcds,     u)
  u={}; for k,abcd1 in pairs(abcds.all) do u[1+#u] = {
    _n   = abcd1.a+abcd1.b+abcd1.c+abcd1.d,
    _a   = abcd1.a, 
    _b   = abcd1.b, 
    _c   = abcd1.c, 
    _d   = abcd1.d,
    acc  = l.accuracy(abcd1),
    prec = l.precision(abcd1), 
    pd   = l.recall(abcd1),     
    pf   = l.pf(abcd1),
    f    = l.f(abcd1),           
    g    = l.g(abcd1)} end 
  return end
