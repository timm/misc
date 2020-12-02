#!/usr/bin/env ./auk
#- vim: ft=awk ts=2 sw=2 et :

BEGIN { go()
        eg1( "weather")
        stop() }

# Holds rows, summarized into columns
function Rows(i) {
  Obj(i)
  has(i,"rows")
  has(i,"cols") }

# Generic column
function Col(i,pos,txt) {
  Obj(i)
  i.pos = pos
  i.txt = txt
  i.w   = (txt ~ /</) ? -1 : 1  }

# Handles symbolic columns
function Sym(i,pos,txt) { 
  Col(i,pos,txt)
  i.is = "Sym" }

# Handles numeric columns
function Num(i,pos,txt) { 
  Col(i,pos,txt)
  i.is = "Num"
  i.lo =  1E32
  i.hi = -1E320 }

# Decides our column type
function what(t)  { return x ~ /[<>]/ ? "Num" : "Sym" } 

# First addition is the header, rest are data
function add(i,a) { length(i.cols) ? data(i,a) : head(i,a) }

# Create numeric, symbolic columns
function head(i,a,   j) {
  for(j in a) 
    hAS(i.cols,j,what(a[j]),j,a[j])}

# Add a new row
function data(i,a,  r,j) { 
  r= length(i.rows) + 1
  for(j in a)  
    i.rows[r][j] = a[j] }

function eg1(f,   i) {
  Rows(i)
  reads(i,"data/" f D "csv")
  oo(i) }

#--- misc function
function go() { D=sprintf("%c",46)  }
function stop()  { rogues() }

function Obj(i) { i[0]; delete i[0] }

function has(i,k,f)      { i[k][0]; delete i[k][0]; if(f) @f(i[k])     } 
function haS(i,k,f,x)    { i[k][0]; delete i[k][0]; if(f) @f(i[k],x)   } 
function hAS(i,k,f,x,y)  { i[k][0]; delete i[k][0]; if(f) @f(i[k],x,y) } 

function say(a,  i) { for(i in a) printf("-- %s",a[i]); print "" }

function oo(a,prefix,    gap,   i,t) {
  t = gap ? gap : (prefix D )
  if (!isarray(a)) {print(a); return 1}
  for(i in a)  
    if  (isarray(a[i])) {print(t i"" ); oo(a[i],"","|  " gap) } 
    else                 print(t i (a[i]==""?"": ": " a[i]))  }

function rogues(   s,ignore) { 
  for(s in SYMTAB) 
    if (s ~ /^[_a-z]/) 
      print "#W> Rogue: "  s>"/dev/stderr" }

#function new(i,f,     k) { k=length(i)+1; has(i,k,f);     return k }
#function neW(i,f,x,   k) { k=length(i)+1; haS(i,k,f,x);   return k }
#function nEW(i,f,x,y, k) { k=length(i)+i; haS(i,k,f,x,y); return k }

function reads(i,f,    a) {
  f  = f ? f : "-"
  FS = ","
  while ((getline < f) > 0) { readrow(a); add(i,a) }}

function readrow(a,   j,tmp) {
  for(j=1;j<=NF;j++) {
    tmp  = $j + 0
    a[j] = $j == tmp ? tmp : $j }}


