BEGIN {
   COHEN=0.35
   REPEATS=100
   srand(1)
   for(b=10;b<100;b+=10)
    for(s=250;s<=1000;s += 250)
       main(b,s) }

function main(budget,samples,    r,good) {
  r=REPEATS
  while (r--) good += run(budget,samples) 
  print(budget, samples,int(100*good/REPEATS)) | "/usr/bin/sort -n -k 3" }

function run(budget,samples,      i,a,best,enough,R) {
   for(i=1;i<=samples;i++) a[i] = rand()^2
   enough = stats(a)
   for(i=1;             i < int(budget/2); i++) best = max(best, a[i]) 
   for(i=int(budget/2); i <= length(a) ; i++) if (a[i] > best) return  a[i]  > enough }
     
function max(a,b) { return a>b ? a : b } 

function stats(a,    b,n,sd,top) {
  n  = asort(a,b)
  n  = int(n/10)
  sd = (b[n*9] - b[n])/2.56
  top= b[length(b)]
  return   top - sd*COHEN }
