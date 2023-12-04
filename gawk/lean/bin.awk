@include "lib" 

BEGIN{ BINS = 5
       MIN = .5
       FS = OFS = "," }
     { row()  }
END  { main() }

function row(     i,num) {
  for(i=1;i<=NF;i++) $i=trim($i)
  if(NR==1) {
    print;
    for(i=1;i<=NF;i++) 
      if ($i ~ /^[A-Z]/) num[i] 
  } else {
    for(i=1;i<=NF;i++)
      if (i in num) Seen[i][NR] = ($i+=0)
     Data[NR][i] = $i }}

function main(     i) {
  for(i in Seen) {
    bins(i,Seen[i]) }
  rogues() }

function bins(col,a,    max,jump,small,i,lo,x,b) {
   max   = asort(a,b)
   jump  = int(max/BINS)
   small = D*sd(b)
   i     = jump 
   lo    = b[1]
   while(i < max) {
     x = b[i]
     if ((x != b[i+1]) && (x - lo) > small) {
       Cuts[col][x] = x
       lo  = x
       i  += jump }
     else
       i++ }}
 

     

