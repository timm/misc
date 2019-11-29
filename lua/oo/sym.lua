-- vim: nospell:sta:et:sw=2:ts=2:sts=2

require "thing"

Sym={show="Sym"}

function Sym.new(i)
  return also(i,
    Thing.new{ me=Sym, seen={}, mode=nil,most=0})
end

function Sym.prep(x) return x end

function Sym.add1(i,x)
  local new= (i.seen[x] or 0) + 1
  i.seen[x] = new
  if new > i.most then i.most, i.mode = new, x end
end

