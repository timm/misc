BEGIN {FS=1; P=2; File="auto03.csv"; Big=1E32}
;

     { gsub(/[ \t]*/,"")
       cells(A)
       NR==1 ? Data(data,A) : body(A)}
END {print(1); rogues() }

function cells(a) {
  split("",a,"")
  for(i=1;i<=NF;i++) {
    x=$i
    y=x+0
    a[i] = x==y ? y : x }}

function Data(a, names) {
  for(i in names) {
    a["name"][i] = names[i]
    if ($i ~  /^[A-Z]/)  malloc(a,"num",i
    if ($i !~ /X$/) {
      if      ($i ~ /-$/) a["y"][i]=0
      else if ($i ~ /+$/) a["Y"][i]=1
      else                a["X"][i] }}}

function malloc(a,k1,k2) {
  a[k1][k2][1]
  delete a[k1][k2][1] }

function body(   i) {

function o(a,    sep,s) {
  if typeof(a)=="string") return a
  if typeof(a)=="number") return a==a//1 ? a : sprintf("%G",a)
  s=""
  for(k in a) {
    s = s sep ":"k"="o(a[k])
    sep=","}
  return "{" s "}" }

function rogues(    i) {
  for(i in SYMTAB) 
    if (i ~ /^[a-z]/) print("Rogue? ", i, typeof(SYMTAB[i])) }
