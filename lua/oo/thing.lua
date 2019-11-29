-- vim: nospell:sta:et:sw=2:ts=2:sts=2

require "object"

Thing={up=Object, show="Thing"}

function Thing.new(i) 
  return also(i,
    Object.new{me=Thing, 
               n=0,pos=100, w=1,
               txt="",all={},key=same})
end

function Thing.add(i,x,    y) 
  y = i.key(x)
  if y ~="?" then
    y = i.me.prep(y)
    i.n = i.n+1
    i.me.add1(i,y) end
  return y
end
