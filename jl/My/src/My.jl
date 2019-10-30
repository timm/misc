module My
using Parameters 
using Random

Random.seed!(1)

same(s) = s
anyi(s) = floor(Int,round(s*rand()))
any(a)  = a[ anyi(size(a)[1]) ]

@with_kw mutable struct Config
  char = (skip='?',less='>',more='<',num='$',klass='!')
  str  = (skip="?",)
  some = (max=256,step=.5, cohen=.3, trival=1.05)
end

THE = Config()

adds!(init=[],i=Some) = incs!(i(),init, 1)
subs!(init=[],i=Some) = incs!(i(),init,-1)
add!(i,x)             = inc!( i  ,   x, 1)
sub!(i,x)             = inc!( i  ,   x,-1)

function incs!(i,init, w)
  [inc!(i,x,w) for x in init]
  i
end

function inc!(i,x,w)
  y=i.key(x)
  if y != THE.char.skip 
    i.n += w
    inc1!(i, y,w) end
end

@with_kw mutable struct Some 
  pos=0; txt=""; w=1; key=same; n=0;
  all=[]; max=THE.some.max ;sorted=false end

function contents!(s::Some)
  if (!s.sorted) sort!(s.all,by=s.key); s.sorted=true end
  s.all
end

p(i::Some,n)      = contents!(i)[ floor(Int,n*length(i.all)) + 1 ]
mid(i::Some)      = p(i,0.5)
coerce(i::Some,x) = x isa Number ? x : tryparse(Float64,x)
function var(i::Some,lo=0,hi=1)      
  lo  = floor(Int,lo*length(i.all)) + 1
  hi  = floor(Int,hi*length(i.all)) + 1
  p10 = floor(Int,lo + (hi - lo) *.1) + 1
  p90 = floor(Int,lo + (hi - lo) *.9) + 1
  return (i.all[p90] - i.all[p10])/2.7

function inc1!(i::Some, x,_)
  i.sorted=false
  m = length(i.all)
  if m < i.max
    push!(i.all,x)
  elseif rand() < m/i.n
 i.all[ floor(Int,m*rand()) + 1 ] = x end end

#
#def div(i,step=.5, cohen=.3, trival=1.05):
#    def go(lo, hi, rank,  cuts,cut=None):
#      best = i.var(lo,hi)
#      for j in range(lo,hi):
#        if j - lo >= step:
#         if hi - j >= step:
#           now = i.x(j)
#           after=i.x(j+1)
#           if now == afer: continue
#           if  after - start > epsilon:
#             if stop - now   > epsilon:
#               if abs(i.mid(lo,j) - i.mid(j,hi)) > epsilon:
#                 xpect = i.xpect(lo,j,hi)
#                 if xpect*trivial < best:
#                   best,cut = xpect,j+1
#      if cut:
#        rank = go(lo,cut,rank, cuts) + 1
#        rank = go(cut,hi,rank, cuts)
#      else:
#        cuts += [ [i.has[z] for z in range(lo,hi)] ]
#      return rank
#    n               = len(i.has)
#    step,start,stop = n**step, i.x(0), i.x(n)
#    epsilon         = i.var(0,n)*cohen
#    cuts            = []
#    go(1, n, 1, cuts)
#    return cuts
#
@with_kw mutable struct Sym 
  pos=0; txt=""; w=1; key=same; n=0;
  seen=Dict(); ent=nothing end

mid(i::Sym) = i.mode
coerce(i::Sym,x) = x

function var(i::Sym) 
  if i.ent == nothing
    i.ent = sum( -n/i.n*log(2,n/i.n) for (_,n) in i.seen ) end
   i.ent
end

function inc1(i::Sym,x,w)
  i.ent = nothing
  old   = haskey(i.seen, x) ? i.seen[x] : 0
  new   = old + w
  if new <0 new=0 end
  i.seen[x] = new
end

@with_kw struct Lines file; src=open(file) end

"Define an iterator that returns a comma-seperated file, one
 record at a time without loading the whole file into memory."
function Base.iterate(it::Lines, (n,want)=(1,[]))
  "Split on comma, coerce strings to numbers or strings, as approriate." 
  coerce(s)  = map(coerce1, split(s,","))
  coerce1(s) = ((x = tryparse(Float64,s))==nothing) ? s : x

  "Coerce strings. If first row, check what columns we should use.
   Only return those columns."
  function cols(a)
    if n == 1 
      want = [i for (i,s) in enumerate(a) if !('?' in s)] end
    [a[i] for i in want]  
  end
  
  "Delete comments and whitespace. Lines ending in
   ',' are joined to the next. Skip empty lines."
  function row(txt="")
    while true
      if eof(it.src) return txt end
      new = readline(it.src)
      new = replace(new, r"([ \t\n]|#.*)"=>"")
      if sizeof(new) != 0 
        txt *= new
        if txt[end] != ',' 
	  return txt end end end end 

  new = row()
  if sizeof(new) > 0 
    (n, cols(coerce(new))) , (n+1,want) end 
 end

#--------------------------------------------
# Tbl
@with_kw mutable struct Tbl
  rows=[]; cols=Cols() end

@with_kw mutable struct Row
 cells=[]; cooked=[] end

@with_kw mutable struct Cols
  x = (all=[], nums=[], syms=[])
  y = (all=[], nums=[], syms=[], goals=[])
  klass=""
  all  = []; nums = []; syms = []; end

function table(file::String) 
  t=Tbl()
  for (n,a) in Lines(file=file)
    n==1 ? head!(t,a) : row!(t,a) end
  t
end

function row!(i::Tbl,a) 
  [add!(c,a[c.pos]) for c in i.cols.all]
  push!(i.rows, Row(cells=a) )
end

head!(i::Tbl,a) = [head!(i.cols,n,x) for (n,x) in enumerate(a)]

function head!(i::Cols, n,txt)
  the = THE.char
  goalp()  = the.less in txt || the.more in txt 
  nump()   = the.num  in txt || goalp() 
  yp()     = klassp() || goalp() 
  klassp() = the.klass in txt 
  x = nump() ? Some : Sym 
  y = x(pos=n, txt=txt)
  if klassp() i.klass = y end
  if goalp()  push!(i.y.goals, y) end
  if nump()
    push!(i.nums,y); push!(yp() ? i.y.nums : i.x.nums, y)
  else
    push!(i.syms,y); push!(yp() ? i.y.syms : i.x.syms, y)
  end
  push!(yp()  ? i.y.all : i.x.all, y)
  push!(i.all, y)
end

#--------------------------------------------

function tbl1()
  t = table("data/xomo10000.csv")
  #println(t.rows[end].cells)
  print(length(t.rows))
end

function sym1()
  s=Sym()
  [add!(s,x) for x in "aaaabbc"]
  println(">> ", s.seen['a'], " ", var(s))
end

function some1()
  s=Some(max=32)
  for j in 1:10000 add!(s,j) end
  println(contents!(s))
  println(var(s))
end

function Lines1()
  m=1
  print(m)
  for (n,tmp) in Lines(file= "data/xomo10000.csv")
     m += sizeof(tmp)  #println(n," ",tmp)
     if mod(n,1000) == 0 println(n,":",m) end
  end
  print(m)
end

#some1()
#sym1()
tbl1()
end
