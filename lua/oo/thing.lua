-- vim: nospell:sta:et:sw=2:ts=2:sts=2

require "lib"
Thing = Object:new{n=0, pos=0, w=1, txt="", key=same}
Sym   = Thing:new{ counts={}, mode=nil, most=0}

function Thing:prep(x) return x end
function Thing:add(x)
  x = self.same(x) 
  if x ~= "?" then
    x = self:prep(x)
    self.n = self.n + 1
    x:add1(x) 
  end 
end

function Thing:add1(x)
  print(x) --
end
