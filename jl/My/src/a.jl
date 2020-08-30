using Parameters
using Random

@with_kw mutable struct Config
  char = (skip='?',less='>',more='<',num='$',klass='!')
  str  = (skip="?",)
  some = (max=32,)
  div  = (divs=32, cohen=0.3, trivial=1.05)
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

function div(lst, x=same)
  lst     = sort([y for y in lst if x(y) != the.str.skip], by=x)
  at(z,a) = x( a[ int(z)+1 ] )
  var(a)  = (at(.9*length(a),a) - at(.1*length(a),a)) / 2.7
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
        v12, v3    = n1/n3*var(one) + n2/n3*var(two), var(three)
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
  merge( chop(lst, the.div.cohen * var(lst)) )
end

function main()  
  lst = [int(100*round(rand()^.5,digits=2)) for _ in 1:256]
  for (n,one) in enumerate(div(lst))
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
