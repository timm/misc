using Parameters
using Random

@with_kw mutable struct Config
  char = (skip='?',less='>',more='<',num='$',klass='!')
  str  = (skip="?",)
  some = (max=32,)
  div  = (divs=16, cohen=0.3, trivial=1.05)
  seed = 1
end

the=Config()
Random.seed!(the.seed)

same(s) = s
int(x)  = floor(Int,x)
any(a)  = a[ int(length(a) * rand()) + 1]

function say(i)
  s,pre="$(typeof(i)){",""
  for f in sort!([x for x in fieldnames(typeof(i)) if !("$x"[1] == '_')])
    g = getfield(i,f)
    s = s * pre * "$f=$g" 
    pre=", "
  end
  print(s * "}")
end

@with_kw mutable struct Num  
  pos=0; txt=""; w=1; n=0; lo=10^32; hi=-1*10^32; mu=0; m2=0; sd=nothing 
end
@with_kw mutable struct Some 
  pos=0; txt=""; w=1; n=0; _all=[]; max=the.some.max; stale=false 
end
@with_kw mutable struct Sym  
  os=0; txt=""; w=1; n=0; seen=Dict();  mode=nothing; ent=nothing  
end 

incs!(i,inits)      = begin [inc!(i,x) for x in inits]; i end
nump(s,c=the.char)  = c.less in s || c.more in s || c.num in s
goalp(s,c=the.char) = c.less in s || c.more in s || c.klass in s

function col(txt,pos)
   x = nump(txt) ? Num : Sym
   x(txt=txt, pos=pos, w= the.char.less in txt ? -1 : 1)
end

function all(i::Some)
  if i.stale
    i._all = sort(i._all)
    i.stale=false
  end
  return i._all
end

function inc!(i::Some, x)
  m = length(i._all)
  if m < i.max
    i.stale=true
    push!(i._all,x)
  elseif rand() < m/i.n
    i.stale=true
    i._all[ int(m*rand()) + 1 ] = x end 
end

function var(i::Num)
  if     i.m2 < 0  0 
  elseif i.n  < 2  0 
  else             (i.m2 / (i.n - 1 + 10^-32))^0.5 
  end
end

function inc!(i::Num,x)
  i.lo  = min(i.lo, x)
  i.hi  = max(i.hi, x)
  d     = x - i.mu
  i.mu += d / i.n
  i.m2 += d * (x - i.mu)
end

function div(lst, x, y)
  lst     = sort([z for z in lst if x(z) != the.str.skip], by=x)
  sd(a,f=x) = var( incs!( Num(), [f(z) for z in a]))
  function chop(a, eps)
    m = length(a)
    tmp, out, n = [], [], m / the.div.divs
    last=nothing
    for (i,one) in enumerate(a)
      if m - i > n && x(one) != x(last) 
        if length(tmp) >= n  && x(one) - x(tmp[1]) > eps
          push!(out, tmp)
          tmp = [] 
      end end
      push!(tmp,one)
      last = one
    end
    if length(tmp) > 0 push!(out,tmp) end
    out
  end
  function merge(a)
    tmp, out, j, m = [], [], 1, length(a)
    while j <= m
      one = a[j]
      if j < m
        two   = a[j+1]
        three = [ one;two ]
        n1, n2, n3 = length(one), length(two), length(three)
        v12, v3    = n1/n3*sd(one,y) + n2/n3*sd(two,y), sd(three,y)
        if v3*the.div.trivial < v12
          one = three
          j += 1
        end 
      end
      push!(tmp,one)
      j += 1
    end 
    return length(tmp) < length(a) ? merge(tmp) : a
  end
  merge( chop(lst, the.div.cohen * sd(lst,x)) )
end

function main()  
  lst = [int(100*round(rand()^.5,digits=2)) for _ in 1:256]
  lst = [[x, x<30 ? 1 : 0] for x in lst]
  print(lst)
  first(z)  = z[1]
  second(z) = z[2]
  for (n,one) in enumerate(div(lst,first,second))
     println("\n",n,") ", length(one), " ",one) 
  end
end

main1() = for _ in 1:10; print(any([1,2,3])) end

try
  println("\n--| ",int(time() % 1000)," |----------------")
  main()
catch e
   showerror(stdout, e, catch_backtrace()[1:10])
   exit(1)
end
