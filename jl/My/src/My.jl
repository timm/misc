module My
using Parameters 
using Random

Random.seed!(1)

same(s)  = s
anyi(s)  = floor(Int,round(s*rand()))
any(lst) = lst[ anyi(size(lst)[1]) ]

@with_kw mutable struct Config
  char = (skip='?',)
  str  = (skip="?",)
  some = (max=256,)
end

THE = Config()

@with_kw mutable struct Cols
  x  = (all  = [], nums = [], syms = [])
  y = (all  = [], nums = [], syms = [],
           goals= [22], klass= nothing)
  all  = [] 
  nums = [] 
  syms = (10, 20,   30, 40)
  name= ""
  pos=0
end

z=Cols(name="jane", pos=2)
#show(z)
#w = Cols(),
#dump(w)
#dump(nothing)


@with_kw mutable struct Some 
  key=same; all=[]; n=0; max=THE.some.max ;sorted=false;
end

function adds(t, lst=[], key=same)
  u = t(key=same)
  [add(u,x) for x in lst]
  u
end

function add(s,x)
  y = s.key(x)
  if y != THE.char.skip 
    s.n += 1
    add1(s,x) end
  y
end

function contents(s::Some)
  if !s.sorted 
    sort!(s.all,by=s.key); s.sorted = true end
  s.all
end

len(i::Some)   = size(i.all)[1]
p(  i::Some,n) = contents(i)[ floor(Int,n*len(i)) + 1 ]
var(i::Some)   = (p(i,0.9) - p(i,0.1))/2.7
    
function add1(i::Some, x)
  i.sorted=false
  m = len(i)
  if m < i.max
    push!(i.all,x)
  elseif rand() < m/i.n
    r = floor(Int,m*rand()) + 1
    i.all[ r ]= x end end

@with_kw mutable struct Sym 
  seen=Dict();  n=0; ent=nothing; most=0; mode="";key=same;
end

function var(i::Sym)
  if i.ent == nothing
    i.ent = 0
    for (_,n) in i.seen
      p = n/i.n
      i.ent -= p*log(2,p) end end
   i.ent
end

function add1(i::Sym,x)
  i.ent = nothing
  old = haskey(i.seen, x) ? i.seen[x] : 0
  new = i.seen[x] = old + 1
  println(x," ",i.seen[x])
  if new > i.most
    i.most, i.mode = new, x end
end

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

some1()
sym1()

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

function Lines1()
  m=1
  print(m)
  for (n,tmp) in Lines(file= "data/xomo10000.csv")
     m += sizeof(tmp)  #println(n," ",tmp)
     if mod(n,1000) == 0 println(n,":",m) end
  end
  print(m)
end

function demo()
  print(444)
  open("src/My.jl") do file
      n=1
      ld=""
      function prep(s)
        if sizeof(s) > 0
          println( map(num, split(s,",")))end end
      for line in eachline(file)
          line = replace(line, r"([ \t\n]|#.*)" => "")
          if sizeof(line) == 0  continue end
          print(line[end])
          if line[end] == ',' 
            old = old * line
          else
            prep(old*line)
            old = ""
          end 
      end
      prep(old)
  end
end 
end
