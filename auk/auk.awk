# auk.awk
# built-ins for the auk language
# should be all "raw" gawk (no auk extensions)

BEGIN {
   Gold["dot"] = sprintf("%c",46) 
   Gold["pi"]  = 3.1415926535
   Gold["e"]   = 2.7182818284
   Gold["id"]  = 0 }

#--------------------------------------------------------------------
### transpile stuff
function auk2awk(f,  klass,tmp) {
  while (getline <f) {
    # multi line comments delimited with #< ... >#
    if(/^#</) {  
      print $0; 
      while(getline<f)  { print "# "$0; if(/^#>/) break } 
    }
    # grab class name so we can expand "_" to current class
    if (/^function[ \t]+[A-Z][^\(]*\(/) {  # new class name
      split($0,tmp,/[ \t\(]/); klass = tmp[2] 
    }
    # expand "_" to the current class
    gsub(/ _/," " klass)
    # turn a.b.c[1] into a["b"]["c"][2]
    print  gensub(/\.([^0-9\\*\\$\\+])([a-zA-Z0-9_]*)/, 
                  "[\"\\1\\2\"]","g", $0) }}

#--------------------------------------------------------------------
### object creation stuff
function nested(i,k) { i[k]["\001"]; delete i[j]["\001"] }

function Obj(i)  {  i["id"] = ++Gold["id"] }

## add a nested list to `i` at `k` using constructor `f` (defaults to `Obj`)
## the haS and hAS variants are the same, but constructors have 1 or 2 args
function has(i, k, f)       { f=f?f:"Obj"; nested(i,k); @f(i[k])      }
function haS(i, k, f, x)    {              nested(i,k); @f(i[k],x)    }
function hAS(i, k, f, x, y) {              nested(i,k); @f(i[k],x,y)  }

#--------------------------------------------------------------------
### list stuff
## push to end of list
function push(x,a) { a[length(a)+1]=x; return x }

## flat list to string. optionally, show `prefix`
function o(a, prefix,     i,sep,s) {
  for(i in a) {s = s sep prefix a[i]; sep=","}
  return  s }

## print nested array, keys shown in sorted order
function oo(a,prefix,    indent,   i,txt) {
  txt = indent ? indent : (prefix ? prefix Gold["dot"] : "")
  if (!isarray(a)) {print(a); return a}
  ooSortOrder(a)
  for(i in a)  {
    if (isarray(a[i]))   {
      print(txt i"" )
      oo(a[i],"","|  " indent)
    } else
      print(txt i (a[i]==""?"": ": " a[i])) }}

function ooSortOrder(a, i) {
  for (i in a)
   return PROCINFO["sorted_in"] =\
     typeof(i+1)=="number" ? "@ind_num_asc" : "@ind_str_asc" }

#--------------------------------------------------------------------
### meta stuff
## hunt down rogue local variabes
function rogues(   s,ignore) { 
  for(s in SYMTAB) 
    if (s ~ /^[_a-z]/) 
      print "#W> Rogue: "  s>"/dev/stderr" }

#--------------------------------------------------------------------
### file stuff
## looping over file stuff
function csv(a, f,       b4, g,txt,i,old,new) {
  f = f ? f : "-"             
  g = getline < f
  if (g< 0) { print "#E> Missing f ["f"]"; exit 1 } # file missing
  if (g==0) { close(f) ; return 0 }       # end of file                   
  txt = b4 $0                             # combine with prior
  gsub(/[ \t]+/,"",txt)
  if (txt ~ /,$/) { return csv(a,f,txt) } # continue txt into next
  sub(/#.*/, "", txt)                    # kill whitespace,comments    
  if (!txt)       { return csv(a,f,txt) } # skip blanks
  split(txt, a, ",")                      # split on "," into "a"
  for(i in a) {
     old = a[i]
     new = a[i]+0
     a[i] = (old == new) ? new : old
  }
  return 1 } 
