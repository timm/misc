-- vim: nospell:sta:et:sw=2:ts=2:sts=2

function deepcopy(t) 
    return type(t) ~= 'table' and t or collect(t,deepcopy) end

function collect(t,f,  out)
     out={}
     f= f or function (z) return z end
     if t then  
       for i,v in pairs(t) do out[i] = f(v) end end
     return out
end 

Object={}

-- Object.new(self,o)
function Object.new(o,new) 
  for k,v in pairs(new) do
    o[k] = new[k] or o[k]
  end
  return  o
end

Thing={}
 
function Thing.new(i)
  return Object.new({n=0, pos=100, w=1,txt="",all={}},i)
end

function Thing.add(i,x) 
  i.all = i.all or {}
  i.all[#i.all + 1]=x
end
