```lua

function SYM.rnd(i,x,n) return x end --> s; return `n` unchanged (SYMs do not get rounded)

function SYM.dist(i,s1,s2)
  return s1=="?" and s2=="?" and 1 or (s1==s2) and 0 or 1 end 
-- ### NUM
-- Summarizes a stream of numbers.
function NUM.new(i,at,txt) --> NUM;  constructor; 
  i.at, i.txt = at or 0, txt or "" -- column position and name
  i.n, i.mu, i.m2 = 0, 0, 0
  i.lo, i.hi = math.huge, -math.huge 
  i.w = i.txt:find"-$" and -1 or 1 end

function NUM.add(i,n,    d) --> NUM; add `n`, update lo,hi and stuff needed for standard deviation
  if n ~= "?" then
    i.n  = i.n + 1
    d = n - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(n - i.mu)
    i.lo = math.min(n, i.lo)
    i.hi = math.max(n, i.hi) end end

function NUM.mid(i,x) return i.mu end --> n; return mean

function NUM.div(i,x)  --> n; return standard deviation using Welford's algorithm http://.ly/nn_W
    return (i.m2 <0 or i.n < 2) and 0 or (i.m2/(i.n-1))^0.5  end

function NUM.rnd(i,x,n) return x=="?" and x or rnd(x,n) end --> n; return number, rounded

function NUM.norm(i,n)
  return n == "?" and n  or (n - i.lo)/(i.hi - i.lo + 1E-32) end

function NUM.dist(i,n1,n2)
  if n1=="?" and n2=="?" then return 1 end
  n1,n2 = i:norm(n1), i:norm(n2)
  if n1=="?" then n1 = n2<.5 and 1 or 0 end
  if n2=="?" then n2 = n1<.5 and 1 or 0 end
  return math.abs(n1 - n2) end 
```

```julia
@with_kw mutable struct Some
  pos=0; txt=""; w=1; n=0; 
  _all=[]; max=it.some.max; stale=false end

@with_kw mutable struct Sym
  pos=0; txt=""; w=1; n=0; 
  seen=Dict(); mode=no; most=0 end

#---- Create -----------------------------------------------
function col(;txt="",pos=0, c=it.char)
  x= c.less in txt||c.more in txt||c.num in txt ? Some : Sym  
  w= it.char.less in txt ? -1 : 1
  x(txt=txt, pos=pos, w=w) end

#---- Update -----------------------------------------------
inc!(i,x)=
  x==it.char.skip ? x : begin i.n += 1; inc1!(i,x); x end

function inc1!(i::Sym, x)
  new = i.seen[x] = 1 + get(i.seen,x,0)
  if new > i.most
    i.mode, i.most = x,new end end

function inc1!(i::Some, x::Number)
  m = length(i._all)
  if m < i.max 
    i.stale=true
    push!(i._all,x) 
  elseif rand() < m/i.n
    i.stale=true
    i._all[ int(m*rand())+1 ]=x end end

#---- Query ------------------------------------------------
norm!(i::Some,x, a=all(i), skip=x==it.char.skip) =
  skip ? x : max(0,min(1, (x-a[1]) / (a[end] - a[1]+1E-31))) 

mid(i::Some;lo=no,hi=no) = per(i,p=.5,lo=lo,hi=hi)

sd(i::Some;lo=no,hi=no)  = (per(i,p=.9,lo=lo,hi=hi) - 
                            per(i,p=.1,lo=lo,hi=hi)) / 2.564

function per(i::Some;p=.5,lo=no,hi=no, lst=all(i))
  hi = hi==no ? length(lst) : hi
  lo = lo==no ? 1           : lo
  lst[ int(lo + p*(hi - lo +1)) ] end

function all(i::Some)  
  if i.stale i._all=sort(i._all) end
  i.stale=false
  i._all end
````

