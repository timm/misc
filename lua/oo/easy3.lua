
function collect(t,f)
  local out={}
    if t then
        for i,v in pairs(t) do out[i] = f(v) end end
	  return out
	  end

	  function deepcopy(t)
	    return type(t) ~= 'table' and t or collect(t,deepcopy) end


function Object:new()
  i =  setmetatable({},Object)
  Object.__index = Object
  id = id+1
  i.id = id
end

Object = {}
do 
  id = 0
  function Object.new(d)
     local i = new(Object)
     id = id+1
     i.id = id
     for k,v in pairs(d or {}) do i[k] =v  end
     return i
   end
end

function Object:m1()
  print(self.id)
end

x=Object.new()
x:m1()

Thing=Object:new{}

function Thing.new(d)
  local i = new(Thing,{pos=d.pos or 0})
  return i
end

function Thing:m2() print(self.pos,self.id) end

x=Thing.new{pos=20}
x:m1()

