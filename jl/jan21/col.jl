# vim: set et ts=2 sw=2:

@with_kw mutable struct Num
  pos=0; txt=""; w=1; n=0; lo=10^32; hi=-1*10^32; 
  mu=0; m2=0; sd=nothing 
end

@with_kw mutable struct Some
  pos=0; txt=""; w=1; n=0; _all=[]; 
  max=it.some.max; stale=false 
end

@with_kw mutable struct Sym
  os=0; txt=""; w=1; n=0; seen=Dict();  
  mode=nothing; ent=nothing 
end

incs!(i,lst)      = begin [inc!(i,x) for x in lst]; i end
nump(s,  c=it.ch) = c.lt in s || c.gt in s || c.num in s
goalp(s, c=it.ch) = c.lt in s || c.gt in s || c.klass in s

function col(txt,pos)
  x = nump(txt) ? Num : Sym
  x(txt=txt, pos=pos, w= it.ch.lt in txt ? -1 : 1) end

function all(i::Some)
  if i.stale
    i._all = sort(i._all)
  i.stale=false end
  return i._all end

function inc!(i,x)
  if x != it.char.skip
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
    i._all[ int(m*rand()) + 1 ] = x end end

"asdas :: aaasdas  a" 
function var(i::Num)
  if     i.m2 < 0  0
  elseif i.n  < 2  0
  else   (i.m2 / (i.n - 1 + 10^-32))^0.5 end end

function inc1!(i::Num,x)
  i.lo  = min(i.lo, x)
  i.hi  = max(i.hi, x)
  d     = x - i.mu
  i.mu += d / i.n
  i.m2 += d * (x - i.mu) end
