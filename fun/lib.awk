#!/usr/bin/env ./fun
# vim: nospell filetype=awk ts=2 sw=2 sts=2  et :

## Misc utilities.

BEGIN{  DOT=sprintf("%c",46)}

function o(a,t   ,x,s,sep) {
  for(x in a) {
    s= s sep  t"["x"]="a[x]; sep=", " }
  return s
}

function rogues(    s) {
  for(s in SYMTAB) {
     if (s ~ /^[A-Z][a-z]/) print "Global " s
     if (s ~ /^[_a-z]/    ) print "Rogue: " s }
}

function List(i)         { split("",i,"") }
function zap(i,k)        { i[k][0]; List(i[k])} 
function Object(i)       { List(i); i["oid"]=++OID }

function has( i,k,f)     { f=f?f:"List"; zap(i,k); @f(i[k]) }
function has1(i,k,f,m)   {               zap(i,k); @f(i[k],m) }
function has2(i,k,f,m,n) {               zap(i,k); @f(i[k],m,n) }


