# vim: set et ts=2 sw=2:
@with_kw mutable struct Some
  pos=0; txt=""; w=1; n=0; 
  _all=[]; max=it.some.max; stale=false end

@with_kw mutable struct Sym
  pos=0; txt=""; w=1; n=0; seen=Dict();  
  mode=nothing; ent=nothing end

incs!(i,lst)     = begin [inc!(i,x) for x in lst]; i end
nump(s, c=it.ch) = c.less in s || c.more in s || c.num in s
goalp(s,c=it.ch) = c.less in s || c.more in s || c.klass in s

function col(txt,pos)
  x = nump(txt) ? Some : Sym
  x(txt=txt, pos=pos, w= it.ch.lt in txt ? -1 : 1) end

function all(i::Some)
  if i.stale 
    i.stale=false
    i._all = sort(i._all) end
  return i._all end

function per(i::Some;lo,hi)
  lst=all(i)
  lo = lo==nothing ? 1 : lo
  hi = hi==nothing ? length(lst) : hi
  lst[ int(.5*(hi - lo +1)) ] end


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
    i._all[int(m*rand())+1]=x end end
