module My
using Parameters 
using Random

Random.seed!(1)

same(s)  = s
anyi(s)  = floor(Int,round(s*rand()))
any(lst) = lst[ anyi(size(lst)[1]) ]

@with_kw mutable struct Config
  char = (skip='?',less='>',more='<',num='$',klass='!')
  str  = (skip="?",)
  some = (max=256,)
end

THE = Config()

function adds(lst=[], i=Some,key=same)
  j = i(key=same)
  [add(j,x) for x in lst]
  j
end

function add(i,x)
  y=i.key(x)
  if y != THE.char.skip 
    i.n += 1
    add1(i, coerce(i,y)) end
end

@with_kw mutable struct Some 
  pos=0; txt=""; w=1; key=same; n=0;
  all=[]; max=THE.some.max ;sorted=false end

function contents(s::Some)
  if (!s.sorted) sort!(s.all,by=s.key); s.sorted=true end
  s.all
end

p(i::Some,n)      = contents(i)[ floor(Int,n*length(i.all)) + 1 ]
mid(i::Some)      = p(i,0.5)
var(i::Some)      = (p(i,0.9) - p(i,0.1))/2.7
coerce(i::Some,x) = x isa Number ? x : tryparse(Float64,x)

function add1(i::Some, x)
  i.sorted=false
  m = length(i.all)
  if m < i.max
    push!(i.all,x)
  elseif rand() < m/i.n
    i.all[ floor(Int,m*rand()) + 1 ] = x end end

@with_kw mutable struct Sym 
  pos=0; txt=""; w=1; key=same; n=0;
  seen=Dict(); ent=nothing; most=0; mode="" end

mid(i::Sym) = i.mode
coerce(i::Sym,x) = x

function var(i::Sym) 
  if i.ent == nothing
    i.ent = sum( -n/i.n*log(2,n/i.n) for (_,n) in i.seen ) end
   i.ent
end

function add1(i::Sym,x)
  i.ent = nothing
  old   = haskey(i.seen, x) ? i.seen[x] : 0
  new   = i.seen[x] = old + 1
  if new > i.most
    i.most, i.mode = new, x end
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
  function cols(lst)
    if n == 1 
      want = [i for (i,s) in enumerate(lst) if !('?' in s)] end
    [lst[i] for i in want]  end 
  
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
@with_kw mutable struct Row
 cells=[]; cooked=[] end

@with_kw mutable struct Tbl
  rows=[]; cols=Cols() end


@with_kw mutable struct Cols
  x = (all=[], nums=[], syms=[])
  y = (all=[], nums=[], syms=[], goals=[], klass=nothing)
  all  = [] 
  nums = [] 
  syms = []
end

function head(i::Cols, lst,
    skip=THE.char.skip,less=THE.char.less,more=THE.char.more,
    num =THE.char.num, klass=THE.char.klass)
  for (n,w) in enumerate(lst)
    klassp() = klass in w
    goalp()  = less  in w or more in w
    nump()   = num   in w or goal()
    yp()     = klassp() or goalp()
    x        = nump() ? Some : Sym 
    y        = x(pos=n,txt=w)
    if klassp() i.y.klass = y end
    if goalp()  push!(i.y.goals, y) end
    if nump()
      push!(i.nums,y); push!(yp() ? i.y.nums : i.x.nums, y)
    else
      push!(i.syms,y); push!(yp() ? i.y.syms : i.x.syms, y)
    end
    push!(yp()  ? i.y.all : i.x.all, y)
    push!(i.all, y)
  end
end

function row(i::tbl,lst) 
  [add(h,x) for (h,x) in zip(i.cols.all,lst)]
  push!(i.rows, Row(cells=lst) )
end

z=Cols(name="jane", pos=2)
#--------------------------------------------

function sym1()
  s=Sym()
  [add(s,x) for x in "aaaabbc"]
  println(">> ", s.seen['a'], " ", var(s))
end

function some1()
  s=Some(max=32)
  for j in 1:10000 add(s,j) end
  println(contents(s))
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

some1()
sym1()
end
