BEGIN {FS=","}

{ gsub(/[ \t]+/,"") }


@include "au"

function csv(i,file,row,header,fs,rs,        
             pat,fs0,rs0,txts,txt,cells,n,what) {
  header = header ? header : row
  fs0  = FS
  rs0  = RS
  n    = 0
  FS   = fs  ? fs : ","
  RS   = rs  ? rs : "\n"
  file = file ? file : "/dev/stdin"
  pat  = "#"DOT"*"
function main( n) {
  while(getline > 0)  {
    gsub(/[ \t\r]*/, "")
    if(n++ == 0)
      for(i=1;i<=NDif (txt) {
       txts = txts txt
       if (txts !~ /,$/) {
         split(txts, cells, FS)
         txts = ""
         what = n ? row : header
         @what(i,n,cells)
         n++ }}}
  close(file)
  FS= fs0
  RS= rs0
}o

unction rogues(    s) {
  for(s in SYMTAB) 
    if (s ~ /^[A-Z][a-z]/) 
      print "#W> Global " s>"/dev/stderr"
  for(s in SYMTAB) 
    if (s ~ /^[_a-z]/    ) 
      print "#W> Rogue: " s>"/dev/stderr"
}
BEGIN { goues() }
