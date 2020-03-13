#!/usr/bin/env ../fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :


@include "lib.fun"
@include "some.fun"

function diff(x,y,      s) {
  Ttest(s)
  return ttest(x,y,s) && hedges(x,y,s)
}

function Num(i) {
  Object(i)
  i.n  = i.mu = i.m2 = i.sd = 0
  i.lo = 10^32
  i.hi = -1*i.lo
  i.add ="Num1"
}

function Num1(i,v,    d) {
  v += 0
  i.n++
  i.lo  = v < i.lo ? v : i.lo
  i.hi  = v > i.hi ? v : i.hi
  d     = v - i.mu
  i.mu += d/i.n
  i.m2 += d*(v - i.mu)
  i.sd  = _NumSd(i)
  return v
}
function _NumSd(i) {
  if (i.m2 < 0) return 0
  if (i.n < 2)  return 0
  return  (i.m2/(i.n - 1))^0.5
}
function Ttest(i,   all) {
  Object(i)
  i.conf  = 95# selects the threshold for ttest
  i.small = 0.3 # threshold for effect size test.
  # Thresholds for ttests at two different confidence levels
  # -- 95% --------------------------
  i[95][ 3]= 3.182; i[95][ 6]= 2.447;
  i[95][12]= 2.179; i[95][24]= 2.064;
  i[95][48]= 2.011; i[95][96]= 1.985;
  # -- 99% --------------------------
  i[99][ 3]= 5.841; i[99][ 6]= 3.707;
  i[99][12]= 3.055; i[99][24]= 2.797;
  i[99][48]= 2.682; i[99][96]= 2.625;
  i.first = 3  # must be smallest index of above arrays
  i.last  = 96 # must be last    index of above arrays
}
function hedges(x,y,s,   nom,denom,sp,g,c) {
  # from http://tiny.cc/fxsize
  nom   = (x.n - 1)*x.sd^2 + (y.n - 1)*y.sd^2
  denom = (x.n - 1)        + (y.n - 1)
  sp    = sqrt( nom / denom )
  g     = abs(x.mu - y.mu) / sp
  c     = 1 - 3.0 / (4*(x.n + y.n - 2) - 1)
  return g * c > s.small
}
function ttest(x,y,s,    t,a,b,df,c) {
  # debugged using https://goo.gl/CRl1Bz
  t  = abs(x.mu - y.mu) / sqrt(max(10^-64,
                                x.sd^2/x.n + y.sd^2/y.n ))
  a  = x.sd^2/x.n
  b  = y.sd^2/y.n
  df = (a + b)^2 / (10^-64 + a^2/(x.n-1) + b^2/(y.n - 1))
  c  = ttest1(s, int( df + 0.5 ), s.conf)
  #print "c",c,"t",t
  return abs(t) > c
}
function ttest1(s,df,conf,   n1,n2,old,new,c) {
  #print "df",df,"conf",conf,"first",s.first,"last",s.last
  #oo(s)
  if (df < s.first)
    return s[conf][s.first]
  for(n1 = s.first*2; n1 < s.last; n1 *= 2) {
     n2 = n1*2
    if (df >= n1 && df <= n2) {
      old = s[conf][n1]
      new = s[conf][n2]
      #print "old",old,"new",new,"df",df,"n1",n1,"n2",n2
      return old + (new-old) * (df-n1)/(n2-n1)
  }}
  return s[conf][s.last]
}

function _ttest(a,n,k,    x,i,na,nk,s) {
  Num(na)
  Num(nk)
  Some(sa)
  Some(sk)
  for(i in a) {
     Num1(na,  a[i])
     Num1(nk,  a[i]*k)
     Some1(sa, a[i])
     Some1(sk, a[i]*k)
  }
  Ttest(s)
  print( "sigDifferent",ttest(na,nk,s),
         "notSmallEffect",hedges(na,nk,s),
         "and", diff(na,nk),
         "cliffs", SomeDiff(sa,sk),
         "n",n,"k",k)
}

function ttestMain(  n,a,k) {
  srand(1)
  for(n=20;n<=100;n+=20)  {
    for(m=1;m<=n;m++) a[m] = rand()^2
    print("")
    for(k=1;k<=1.5; k*=1.02)
      _ttest(a,n,k)  } # exit}
}
