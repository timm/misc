# vim: set ft=awk :
BEGIN { FS  = ","
        PI  = 355/113
        BIG = 1E32; 
        DOT = sprintf("%c",46)

        THE.k     = 1
        THE.m     = 2
        THE.seed  = 1234567891
        THE.train = DOT DOT "/" DOT DOT "/data/auto93"DOT"csv" 

        main() }

#--------------------------------------------------------------------
function Num(i,name,at) {
  i.is = "Num"
  i.name= name
  i.at = at
  i.n  = i.sd = i.mu= i.m2 = 0
  i.goal = name ~ /-$/ ? 0 : 1
  i.hi = -(i.lo = BIG) } 

function Sym(i,name,at) {
  i.is = "Sym"
  has(i,"seen")
  i.most = i.mode }

function Data(i,names) {
  i.is = "Data"
  has(i,"rows")
  havE(i,"cols","Cols",names) }

function Cols(i,names,     v,klass,role,k) {
  i.is = "Cols"
  for(k in names) {
    v = i.names[k] = names[k]
    klass = v ~ /^[A-Z]/ ? "Num" : "Sym"
    haVE(i.all, k, klass, v, k)
    if (v !~ /X$/) { 
      role = v ~ /[!+-]$/ ? "y" : "x"
      i[role][k] = k }}}

function readData(i,f,     a,k,what) {
  what="Data"
  while ((getline < f) > 0) {
    for(k=1; k<=NF; k++) a[k] = coerce(trim($k))  
    @what(i,a)
    what="addData"}
  close(f) }

function cloneData(i,j, newRows,    r) {
  Data(j, i.cols.names)
  for(r in newRows) add(data1,newRows[r]) }

#--------------------------------------------------------------------
function addData(i,a,     r,v,k) {
  r = 1 + length(i.rows)
  for(k in a) 
    add(i.cols.all[k], (i.rows[r][k] = a[k]))  }

function addSym(i,x,     now) {
  if (x=="?") return
  i.n++
  if ((now  = ++i.seen[x]) > i.most) {
     i.most = now
     i.mode = x }}

function  addNum(i,x,     d) {
  if (x=="?") return
  i.n++
  d     = x - i.mu
  i.mu += d/i.n
  i.m2 += d*(x - i.mu) 
  i.sd  = i.n < 2 ? 0 : (i.m2 / (i.n - 1))^0.5
  i.hi  = max(x, i.hi)
  i.lo  = min(x, i.lo) }

#--------------------------------------------------------------------
function likeSym(i,x,prior) {
  return  (i.has[x] + THE.m*prior) / (i.n + THE.m)  }

function likeNum(i,x,_,      v,tmp) {
  v = i.sd^2 + 1/BIG
  tmp = exp(-1*(x - i.mu)^2/(2*v)) / (2*PI*v) ^ 0.5
  return max(0,min(1, tmp + 1/BIG)) }

function loglikeData(i, row, nall, nh,          k,out,prior) {
  prior = (length(i.rows) + THE.k) / (nall + THE.k*nh)
  for(k in i.cols.x) 
    out += _log( like(i.cols.all[k], row[k], prior) )
  return out + _log(prior) }

function _log(n) { return n>0 ? log(n) : 0 }
#--------------------------------------------------------------------
function abs(a)   { return a<0 ? -a : a }
function max(a,b) { return a>b ? a  : b }
function min(a,b) { return a<b ? a  : b }

function normal(mu,sd) {
  return mu + (sd ? sd : 1)*sqrt(-2*log(rand()))*cos(2*PI*rand()) }

function trim(s) {
  sub(/^[ \t]*/,"",s) 
  sub(/[ \t]*$/,"",s) 
  return s}

function coerce(x,   y) { y=x+0; return x==y ? y : x }

function o(a,    j,k,s,pre,sep,keyp) {
  if (typeof(a) == "number") return sprintf("%g",a)
  if (typeof(a) != "array") return a
  for(j in a) { keyp = (j+0 != j); break }
  for(j in a) {
    k =  keyp ? j"="     : ""
    j=="is"   ? pre=a.is : s=s sep k o(a[j])
    sep=", " }
  return pre "(" s ")" }

#--------------------------------------------------------------------
function add(i,x,       f) {f="add"i.is;  return @f(i,x) }
function like(i,a,b,    f) {f="like"i.is; return @f(i,a,b) }

function has(a,k)        { a[k][1];  delete a[k][1] }
function have(a,k,f)     { has(a,k); return @f(a[k]) }
function havE(a,k,f,x)   { has(a,k); return @f(a[k],x) }
function haVE(a,k,f,x,y) { has(a,k); return @f(a[k],x,y) }

function more(a,f,x,y) { haVE(a,1+length(a),f,x,y); }

#--------------------------------------------------------------------
function go_normal(_,i) {
  for(i=1;i<=100;i++) printf("%.3f\n",normal(1,0.2)) | "sort -n | fmt -60 " }

function go_argv(_) { print(o(ARGV)) }

function go_sym(_,       j,s,str) {
  Sym(s)
  str="abbcccc"
  for (j = 1; j <= length(str); j++) add(s,  substr(str, j, 1)) 
  print(o(s)) }

function go_num(_,       j,n) {
  Num(n)
  for(j=1;j<=1000;j++) add(n,normal(10,2))
  print(o(n)) }

function go_data(_,   d,k) {
  readData(d, THE.train)
  print("x",o(d.cols.x)) 
  print("y",o(d.cols.y)) 
  for(k in d.cols.all) print(o(d.cols.all[k])) }

function go_like(_,   d,r) {
  readData(d, THE.train)
  for(r in d.rows)
    loglikeData(d, d.rows[r], 1000,2) }

#--------------------------------------------------------------------
function main(    fails,j,f) {
  srand(THE.seed)
  for(j in ARGV) {
    f = "go_" substr(ARGV[j],3)
    if (f in FUNCTAB) 
      fails += @f(ARGV[j+1]) }
  rogues()
  exit(fails) }

function rogues(    j) {
  for(j in SYMTAB) 
    if (j~/^[a-z_]/) print("? " j) }

# first things first. code something that runs a function names on command line
# code in the repl. sh is your repl 
# code in spirts. 10 lines, run new test(s)
# N-1 globals is better than N
# malloc befre assign.
