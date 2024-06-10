#!/usr/bin/env gawk -f
# <!-- vim: set ts=2 sw=2 sts=2 et: -->
# <img src="marsQueen.png" align=left width=150>
# use "k" for array index  
# use "i" for self
BEGIN { FS="," }
      { line(Data0) }
END   { print(o(Data0[colS][8])) 
        rogues() }

function line(i,     k,a) {
  for(k=1;k<=NF;k++) a[k] = coerce($k)
  NR==1 ? Data(i,a) : addData(i,a,NR) }

#----------------------------------------------------------------------------------------
# ## Polymorphic verbs

# Simple redirects.
function add(i,x,    f) { f="add" i[isA]; return @f(i,x) }
function mid(i,      f) { f="mid" i[isA]; return @f(i)   }
function div(i,      f) { f="div" i[isA]; return @f(i)   }

#----------------------------------------------------------------------------------------
# ## Data

# Summarizes in the `colS`, the  data stored in `rowS`.
function Data(i,a,     w,k) {
  i[isA="isA"] = "Data"
  array(i,rowS="rowS")
  array(i,colS="colS")
  for(k in a) { 
    w=(a[k] ~ /^[A-Z]/ ?"Num":"Sym"); @w(i[colS][k], k, a[k]) }}

function addData(i,a,r,    k,x) {
  for(k in a) {
    x = i[rowS][r][k] = a[k]
    add(i[colS][k], x) }}

#----------------------------------------------------------------------------------------
# ## Col

# What is true about NUMs and SYMs
function Col(i,at,txt,isa) {
  i[isA="isa"] = isa
  i[heaveN="heaveN"] = txt !~ /-$/ 
  i[aT="at"] = at; i[txT="txt"] = txt; i[N="n"] = 0 }

#----------------------------------------------------------------------------------------
# ## Num

# Summarize a stream of numbers
function Num(i,at,txt) { 
   Col(i,at,txt,"Num")
   i[mU="mU"] = i[m2="m2"] = i[sD="sD"] = 0
   i[lO="lO"] =  1e30
   i[hI="hI"] = -1e30 }

function divNum(i) { return i[mU] }
function midNum(i) { return i[sD] }

function addNum(i,x,     d) {
  if (x != "?") {
    i[N]++
    if (x > i[hI]) i[hI]=x
    if (x < i[lO]) i[lO]=x  
    d = x - i[mU]
    i[mU] += d/i[N]
    i[m2] += d*(x - i[mU])
    i[sD]  = (i[m2] / (i[N] - 1 + 1E-30))^0.5 }}

#----------------------------------------------------------------------------------------
# ## Sym

# Summarize a stream of symbols.
function Sym(i,at,txt) { 
   Col(i,at,txt,"Sym")
   array(i, haS="has")
   i[modE="mode"]=""
   i[mosT="most"]=0 }

function divSym(i) { return i[modE] }
function midSym(i) { return entropy(i[haS]) }

function addSym(i,x) {
  if (x != "?") {
    i[N]++
    if (++i[seeN][x] > i[mosT]) {
      i[mosT] = i[seeN][x]
      i[modE] = x }}}

#----------------------------------------------------------------------------------------
# ## Lib

# Measures of diversity.
function entropy(a,   N,e,k) {
  for(k in a) N += a[k]
  for(k in a) e -= a[k]/N * log( a[k] / N ) / log(2)
  return e }

# Turn strings to strings or nums
function coerce(x,  y) { y=y+0; return x==y? y : x } 

# Ensure there is a nested array at `a[k]`.
function array(a,k)    { a[k][0]=1; delete a[k][0] }

# ### Pretty-print

# Print the pretty string
function oo(a,pre) { print(o(a,pre)) }

# Make the pretty string
function o(a,pre,     k) {
  if (typeof(a) != "array") return a
  for(k in a) return pre "(" (k+0==k ? olist(a) : okeys(a)) ")"}

function okeys(a,     k,s,s1) { 
  PROCINFO["sorted_in"] = "@ind_str_asc"
  for(k in a) {s=s s1":"k" "o(a[k]); s1=" "}; return s }

function olist(a,     k,s,s1) { 
  PROCINFO["sorted_in"] = "@ind_num_asc"
  for(k in a) {s=s s1 o(a[k]); s1=" "}; return s }

function rogues(    k) { 
  for(k in SYMTAB) if (k ~ /^[a-z0-9_]+$/) print("rogue var: "k) }

