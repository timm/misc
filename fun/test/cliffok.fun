#!/usr/bin/env ./fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "lib"
@include "some"

function cliffsDeltaSlow(a,b,    tmp,la,lb,x,y,j,k,gt,lt) {
  la = l(a.has)
  lb = l(b.has)
  for(j in a.has)
    for(k in b.has) {
      x= a.has[j]
      y= b.has[k]
      gt += x > y
      lt += x < y}
  tmp=  abs(lt-gt)/(la*lb) 
  #printf("%s ","a "la" b "lb" lt "lt" gt "gt" tmp "tmp)
  return 0.147 < tmp
}
function cliffsDeltaFaster(a,b,   tmp,j,la,lb,n,x,lo,hi,gt,lt) {
  la = sorted(a)
  lb = sorted(b)
  for(j in a.has) {
    x= a.has[j]
    lo= hi= binChop(b.has, x)
    while(lo >= 1 && a.has[lo] >= x) lo--
    while(hi <= n && a.has[hi] <= x) hi++
    lt += la - hi + 1
    gt += lo
  }
  tmp=  abs(lt-gt)/(la*lb) 
  #printf("%s ","a1 "la" b1 "lb" lt1 "lt" gt1 "gt" tmp1 "tmp)
  return 0.147 < tmp
}
function cliffsDeltaFastest(a,b,s,   tmp,j,la,lb,n,x,lo,hi,gt,lt) {
  s=s ? s : 10
  la = sorted(a)
  lb = sorted(b)
  for(j=0; j<=1; j+= 1/s) {
    x = per(a,1,la,j)
    lo = hi = binChop(b.has, x)
    while(lo >= 1 && a.has[lo] >= x) lo--
    while(hi <= n && a.has[hi] <= x) hi++
    lt += la - hi + 1
    gt += lo
  }
  tmp =  abs(gt - lt) / (s*lb)  
  return 0.147 < tmp
}
function z() {return sqrt(-2*log(rand()))*cos(6.2831853*rand())}

function gauss(m,s) { return m + s*z() }
function demo(n,r,   m1,s1, m2,s2,a,b,k,z) {
   srand(1)
   Some(a)
   Some(b)
   m1=10; s1=1
   m2= 5; s2=1
   for(k=1;k<=n;k++)  {
     z = rand() #gauss(m2,s2) #+ gauss(m2,s2)
     Some1(a,z)
     Some1(b,z*r) }
   #print("n",n,"r",r, "slow",  cliffsDeltaSlow(a,b))
   #print("n",n,"r",r, "slow",  cliffsDeltaSlow(a,b), "fast",  cliffsDeltaFaster(a,b))
   print("n",n,"r",r, "slow",  cliffsDeltaSlow(a,b), "fast",  cliffsDeltaFaster(a,b), "fastest",cliffsDeltaFastest(a,b))
}
function demos(   r,j,n) { 
   j=1
   while(j-- > 0) {
     print("") 
     for(n=4;n<=256;n *= 2) 
       for(r=1;r<=1.25;r *= 1.025) 
         demo(n,r) }}
BEGIN { demos(); rogues() }
