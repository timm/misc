BEGIN {
   srand(1)
   main(20,15) }

function main(repeats,budget) {
  while (repeats--) run(budget) }

function run(budget,       i,a,best,n) {
   for(i=1;i<=100000;i++) a[rand()] = rand()^2
   stats(budget,a)
   for(i in a) {
     if(n++ > budget/2) break
     best = max(best, a[i]) }
   for(i in a) 
     if (--n < 0) {
       if (a[i] > best) {
         print(",found," a[i]);
         break }}}
     
function max(a,b) { return a>b ? a : b } 

function stats(budget,a,    b,n,sd,top) {
  n  = asort(a,b)
  n  = int(n/10)
  sd = (b[n*9] - b[n])/2.56
  top= b[length(b)]
  printf "budget," budget ",lo," b[1] ",median," b[n*5]  ",sd," sd  ",hi,"  top ",enough," top - sd*0.35 }
