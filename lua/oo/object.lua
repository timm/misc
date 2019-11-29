-- vim: nospell:sta:et:sw=2:ts=2:sts=2

function same(z) return z end

function call(i,k) return i.me[k]() end

function also(new,old)
  new = new or {}
  for k,v in pairs(new) do old[k] = v end 
  return old
end

Object={show="Object"}

do
  local id = 0
  function new(i)
    id   = id + 1
    i.id = id
    return i  end
end
