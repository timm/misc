#!/usr/bin/env ../fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

BEGIN { chi2init() }
BEGIN { _test(); rogues() }

function _test(     c,d) {
  c[1] = 15;  d[1] = 22; 
  c[0] = 100; d[0] = 93
  print chisq(c,d,95)
}

function chi2init(   i) {
  split("10.83,13.82,16.27,18.47,20.52,22.46,24.32,26.13,"\
        "27.88,29.59,31.26,32.91,34.53,36.12,37.7,39.25,"\
        "40.79,42.31,43.82,45.32,46.8,48.27,49.73,51.18,"\
        "52.62,54.05,55.48,56.89,58.3,59.7",Chi99,",")
   split("3.84,5.99,7.82,9.49,11.07,12.59,14.07,15.51,"\
         "16.92,18.31,19.68,21.03,22.36,23.69,25,26.3,27.59,"\
         "28.87,30.14,31.41,32.67,33.92,35.17,36.42,37.65,"\
         "38.89,40.11,41.34,42.56,43.77",Chi95,",")
   for(i in Chi99) Chi99[i] *= 1
   for(i in Chi95) Chi95[i] *= 1
}

function chisq(o,e,conf,    v,df,chsq, i,c1,c2) {
  df = (length(o) - 1) * (length(e) - 1)
  print("df ",df)
  v  = conf==99 ? interp(Chi99,df) : interp(Chi95,df)
  for(i in o) {
    c1 = o[i]
    c2 = (i in e) ? e[i] : 0
    chsq += (c1 - c2)^2 / c2
  }
  print "chsq " chsq " v " v
  return chsq < v
}

function interp(a,x1,
               xmax,x0,x,y0,y,m,b) {
  xmax = length(a)
  if (x1 > xmax) {
    x0 = xmax
    x  = x0 - 1
    y0 = a[x0]
    y  = a[x]
  } else {
    for(x0=1;x<xmax;x0++) {
      x  = x0 + 1
      y0 = a[x0]
      y  = a[x]
      if(x1 >= x0 && x1 <= x) break }
  }
  m = (y- y0) / (x - x0)
  b = y -  m*x 
  print " x0 " x0 " y0 " y0 " x " x " y " y " m " m " b " b
  return m*x1 + b
}

function rogues(    s) {
  for(s in SYMTAB) if (s ~ /^[A-Z][a-z]/) print "Global " s
  for(s in SYMTAB) if (s ~ /^[_a-z]/    ) print "Rogue: " s
}

