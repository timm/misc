# vim: set et ts=2 sw=2:

@with_kw mutable struct Some
  pos=0; txt=""; w=1; n=0; 
  _all=[]; max=it.some.max; stale=false end

@with_kw mutable struct Sym
  pos=0; txt=""; w=1; n=0; seen=Dict();  
  mode=nil; most=0 end

@with_kw mutable struct Table
  ys=[]; xs=[]; rows=[]; cols=[] end

@with_kw mutable struct Row
  cells=[]; dom=0; y=nil end

incs!(i,lst)     = begin [inc!(i,x) for x in lst]; i end
nump(s, c=it.char) = c.less in s || c.more in s || c.num   in s
goalp(s,c=it.char) = c.less in s || c.more in s || c.klass in s

function col(;txt="",pos=0)
  x = nump(txt) ? Some : Sym
  x(txt=txt, pos=pos, w= it.char.less in txt ? -1 : 1) end

function all(i::Some)
  if i.stale 
    i.stale=false
    i._all = sort(i._all) end
  return i._all end

mid(i::Some;lo=nil,hi=nil) = per(i,p=.5,lo=lo,hi=hi)
sd(i::Some;lo=nil,hi=nil)  = 
  (per(i,p=.9,lo=lo,hi=hi) - per(i,p=.1,lo=lo,hi=hi))/2.54

function per(i::Some;p=.5,lo=nil,hi=nil)
  lst=all(i)
  hi = isnothing(hi) ? length(lst) : hi
  lo = isnothing(lo) ? 1           : lo
  lst[ int(lo + p*(hi - lo +1)) ] end

function inc!(i,x)
  if x != it.char.skip
    i.n += 1
    inc1!(i,x) end
  x end

function inc1!(i::Sym, x)
  new = i.seen[x] = 1 + get(i.seen,x,0)
  if new > i.most
    i.mode, i.most = x,new end end

function inc1!(i::Some, x)
  m = length(i._all)
  if m < i.max
    i.stale=true
    push!(i._all,x) 
  elseif rand() < m/i.n
    i.stale=true
    i._all[int(m*rand())+1]=x end end
