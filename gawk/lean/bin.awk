# globals:  
@include "lib"

BEGIN{ #GLOBALS
       BINS = 5
       MIN = .5
       FS = OFS = ","
       # Col = contents of numeric columns
       # Data= all rows and columns
       # Cuts= splits for data
      }
      { row() }
END   { main()
        rogues() }

function row(     i,num) {
  for(i=1;i<=NF;i++) $i=trim($i)
  if(NR==1) {
    print;
    for(i=1;i<=NF;i++) 
      if ($i ~ /^[A-Z]/) num[i] 
  } else {
    for(i=1;i<=NF;i++)
      if (i in num) Col[i][NR] = ($i+=0)
     Data[NR][i] = $i }}

function main(          i,max,jump,small,j,lo,x) {   
  for(i in Col) {
   max   = asort(Col[i],a)
   jump  = int(max/BINS)
   small = D*sd(a)
   j     = jump 
   lo    = a[1]
   while(j < max) {
     x = a[j]
     if ((x != a[j+1]) && (x - lo) > small) {
       Cuts[j][x] = x
       lo  = x
       j  += jump }
     else
       j++ }}
 

     

