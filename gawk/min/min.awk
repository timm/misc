BEGIN { 
  print 2
  srand(Seed ? Seed : 1234567891) 
  egs() }

function egs(      i,s) {
  print(1)
  for(i in ARGV)  {
    s="eg_" ARGV[i]
    #sub(/^-/,"eg_",s)
    print(s)
    if (s in FUNCTAB) @s() }}

function eg_one() {print 1}

function main(fun,stuff,file,     n,a,i) {
  FS=","
  file = file ? file : "-"
  while (getline file > 0) {
    gsub(/[ \t]*/,"")
    split(a,s,FS)
    for(i=1;i<=NF;i++) if ($i ~ /^[A-Z]/) Nump[i]
    @fun(a,n++,stuff) }
  close(file) }


function oo(a)    { print o(a) }
function o(a,  i) { for(i in a) return "(" (i=="1" ? _ints(a) : _keys(a)) ")" }

function _ints(a,   s,i,sep) {
  for(i in a) {s=s sep a[i]; sep=", "}
  return s }

function _keys(a,   s,i,sep) {
  for(i in a) {s=s sep ":"i " " a[i]; sep=" "}
  return s }

