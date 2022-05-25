using Parameters
using Random
#using BenchmarkTools

@with_kw mutable struct Config
  char = (skip='?',less='>',more='<',num='$',klass='!')
  str  = (skip="?",)
  some = (max=32,)
  div  = (few=1024,divs=16, cohen=0.3, trivial=1.05)
  seed = 1
	w    = 1
	name = ""
end


#Config(at,txt) = w=match(r'-$,txt)==nothing ? - 1 : 1; Config(w=100)
# Config(at=0,txt="aaa") = begin w=match(r"-$",txt)==nothing ? -1 : 1
# 	                             Config(some=at,w=w,name=txt) end
#											 
															 
Config(at=0,txt="aaa")= Config(
                        some=at,w=match(r"-$",txt)==nothing ? 1 : -1 ,name=txt) 
															 
 
the=Config()
Random.seed!(the.seed)

  
same(s) = s
int(x)  = floor(Int,x)
any(a)  = a[ int(length(a) * rand()) + 1]
many(a,n=the.divs.few) = length(a) < n  ? a : [any(a)  for _ in 1:n]
 


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
  pos=0; txt=""; w=1; n=0; lo=10^32; hi=-1*10^32; mu=0; m2=0; sd=nothing  end
	
@with_kw mutable struct Some 
  pos=0; txt=""; w=1; n=0; _all=[]; max=the.some.max; stale=false  end
	
@with_kw mutable struct Sym  
  os=0; txt=""; w=1; n=0; seen=Dict();  mode=nothing; ent=nothing   end 

incs!(i,inits)      = begin [inc!(i,x) for x in inits]; i end
nump(s,c=the.char)  = c.less in s || c.more in s || c.num in s
goalp(s,c=the.char) = c.less in s || c.more in s || c.klass in s

function col(txt,pos)
   x = nump(txt) ? Num : Sym
   x(txt=txt, pos=pos, w= the.char.less in txt ? -1 : 1) end

function all(i::Some)
  if i.stale 
		i._all = sort(i._all) end
  i.stale=false
  i._all end

function inc!(i,x)
  if x != the.char.skip
    i.n += 1
    inc1!(i,x) end
  x end

function inc1!(i::Some, x)
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

function inc1!(i::Num,x)
  i.lo  = min(i.lo, x)
  i.hi  = max(i.hi, x)
  d     = x - i.mu
  i.mu += d / i.n
  i.m2 += d * (x - i.mu)
end

function div(lst, x, y)
  lst       = many(lst,the.div.few)
  lst       = sort([z for z in lst if x(z) != the.str.skip], by=x)
  sd(a,f=x) = var(incs!(Num(), [f(z) for z in a]))
  mid(a,f=x) = f( a[ int(length(a)/2) ] )
  eps = the.div.cohen * sd(lst,x)
  function chop(a)
    m = length(a)
    tmp, out, n = [], [], m / the.div.divs
    last=nothing
    for (i,one) in enumerate(a)
      if length(tmp)>=n && m - i > n && x(one) != x(last) 
        if x(one) - x(tmp[1]) > eps
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
        two       = a[j+1]
        three     = [ one ; two ]
        n1, n2, n3= length(one), length(two), length(three)
        sd1,sd2,sd3= sd(one,y), sd(two,y), sd(three,y)
        sd12      = n1/n3*sd1 + n2/n3*sd2
        if abs(sd1 - sd2) < 0.01 || sd12*the.div.trivial >sd3
          one = three
          j += 1
        end 
      end
      push!(tmp,one)
      j += 1
    end 
    return length(tmp) < length(a) ? merge(tmp) : a
  end
  merge( chop(lst))
end

function main()  
  two(r) = if r<0.2 0 elseif r<0.4 1 elseif r<0.6 0 elseif r<0.8 1 else 0 end
  one(r) = [int(100*r), two(r)]
  lst = sort([rand() for _ in 1:10^8])
  lst = [one(r) for r in lst]
  first(z)  = z[1] 
  second(z) = z[2]
  println(100)
  for (i,one) in enumerate(div(lst,first,second))
    println(i," ",length(one)," ", one[1]," ",last(one))
  end
end

main1() = for _ in 1:10; print(any([1,2,3])) end


# try
#   println("\n--| ",int(time() % 1000)," |----------------")
#   main()
# catch e
#    showerror(stdout, e, catch_backtrace()[1:10])
# end
