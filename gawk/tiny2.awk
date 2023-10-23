#!/usr/bin/env gawk -f 
# vim: set et sts=2 sw=2 ts=2 : 

#     
# tiny2.awk : as little asi as possible
# (c)20231 Tim Menzies
# 
# OPTIONS:
#    -s --seed random seed        = 1234567891
#    -B --Best size of best cache = 20 
#    =b --bins number of bins     = 7

BEGIN { the["bins"]=7
        the["Best"]=20
        the["file"]="../data/auto93.csv"
        the["help"]=0
        the["seed"]=1234567891 
        the["go"] = "go"
        main(the) }

#------------------------------------------------
func head(       c) {
  for(c=1;c<=NF;c++) {
    Name[c]=$c 
    if     ($c ~ /-$/)    Ys[c]=0 
    else { if ($c ~ /+$/) Ys[c]=1 
           else           Xs[c]}
    if ($c ~/^[A-Z]/) {Hi[c]= -1e30; Lo[c]=1e30}}}

func read(file      at,tmp,c,_,seen) {
  srand(1)
  while((getline >0) < file) {
    if (seen++ == 0)
      head()
    else { 
      at=rand()
      for(c=1;c<=NR;c++) D[at][c] = coerce($c)
        update(_,NR-1,D[at],Xs) }
  close(file)}

func update(_,seen,a,want,     c,v) {
  for(c in want) {
    v = a[c]
    if (v != "?") {
      N[c]++
      c in Lo ? updateNum(_,c,v,seen) : Has[c][v]++ }}}

func updateNum(_,c,v,seen) {
  if (c in Lo) {
     Mu[c] += (v - Mu[c]) / seen
     if (v < Lo[c]) Lo[c] = v
     if (v > Hi[c]) Hi[c] = v 
     Sd[c] = (Hi[c] - Lo[c]) /6}}

func bin(_,klass,c,v) {
  if (v!="?")
    v = c in Lo ? z(_,v, c) : v
    Freq[klass][c][v]++ }
  
function z(_,v,c,b) {
  b = the["bins"]
  v = int(3.5 + (v - Mu[c]) / Sd[c])
  return v<1 ? 1 : (v>b ? b: v) }

    #klass=klassify(N,ds,a,ys,lo,hi)

func klassify(_,seen,a,    top,klass,at) {
  if (at = better(d2h(_,a))) 
     return at <= sqrt(seen)*1.5 - 1  }

function norm(x,i) {
  return (x - Lo[i]) / (Hi[i] - Lo[i] + 1e-30) }
  #
     #[ys[u] contains 1 0]
func d2h(a,    y,d) {
  for(y in Ys) d += (Ys[y] - norm(a[y],y))**2 
  return (d/length(Ys))**.5}

#-------------------------------------------
func main(a,   f,j) {
  cli(a)
  if (a["help"]) {print(help("tiny2.awk")); exit} 
  else for(j in ARGV) eg("eg_" ARGV[j]) }

func cli(a,     f,j,k) {
  for(k in a) 
    for(j in ARGV) {
      f=ARGV[j]
      if ((f == ("--"k)) || (f==("-"substr(k,1,1))))
        a[k] = a[k]==0 ? 1 : (a[k]==1 ? 0 : coerce(ARGV[j+1])) }}

func help(file,  rs,s) {
  rs=RS; RS=""; getline s<file; getline s<file; close(file);RS=rs
  gsub(/# /,"",s)
  return s }

func least(x,a,max,   n,j) {
  n=length(a)
  if (n < max)  j = n+1 
  if (x < a[n]) j = n
  if(j) {
    a[j] = x
    asort(a)
    return bsearch(a,x)}}

func bsearch(a,x,     lo,hi,mid){  
 	lo=1;
 	hi=length(a);
 	while(lo<hi){
    mid=int((lo+hi)/2);
    if   (a[mid]==x) return mid;             		
    else if (a[mid]>x)  hi=mid-1;
    else lo=mid+1 }
  return hi }

func coerce(x,    num) { num=x+0 ; return x==num ? num : x  }
func max(x,y) { return x>y ? x : y }

func cat(a,pre,sep,s) {
  for(i in a) { s=s sep a[i]; sep=", "}
  print("{"s"}") }

func kat(a,pre,sep,s) {
  for(i in a) { s=s sep ":"i " " a[i]; sep=", "}
  print("{"s"}") }

func new(a) { split("",a,"")}

#------------------------------------------------------
func eg(fun,     status) {
  if (fun in FUNCTAB) {
    srand(the["seed"])
    print("==> ",fun)
    status = @fun()
    if (typeof(status)=="number" && status==0) {
      print("❌ FAILED ", fun); return 1}}}

func eg_all(    fails,fun) {
  for (fun in FUNCTAB) 
    if (fun ~ /^eg_/ && fun != "eg_all") fails += eg(fun)
  exit(fails) }

func eg_the() {kat(the)}
func eg_fail() {return 0}

func eg_vars(  i) {
  for(i in SYMTAB) if (i !~ /^[A-Z][A-Z]/) print("?",i) | "sort"}

func eg_bsearch(     a,i) {
  for(i=1;i<=10;i++) a[i]=10*i  
  for(i=0;i<=120;i=i+10) print(i, bsearch(a,i))
  kat(a)}

func eg_better(     a,i,k) {
  new(a); for(i=1;i<=50;i++) { 
    k=int(rand()*10000)
    print(k, " ",least(k,a,5)) }; kat(a)  }
