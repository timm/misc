-- vim: nospell:sta:et:sw=2:ts=2:sts=2

Object = {}

function show(i,  str,sep)
  str,sep = "{",""
  for k,v in pairs(i) do 
    if type(v) ~= "function" then
      str = str..sep..tostring(k)..":"..tostring(v)
      sep = ", "
    end 
  end
  return str .. "}"
end

function collect(t,f)
  local out={}
  if t then  
    for i,v in pairs(t) do out[i] = f(v) end end
  return out
end 

function deepcopy(t) 
  return type(t) ~= 'table' and t or collect(t,deepcopy) end


function Object:new (o)
  o = deepcopy(o) or {}
  setmetatable(o, self)
  self.__index = self
  self.__tostring = show
  return o
end

Account = Object:new{balance = 0,all={}}

function Account:push(v) 
  self.all[#self.all+1] = v
end
function Account:deposit(v) 
  self.balance = self.balance + v end
    
function Account:withdraw (v)
  if v > self.balance then 
    error"insufficient funds" end
  self.balance = self.balance - v
end

a=Account:new()
b=Account:new()
a:deposit(100)
b:deposit(200)
b:push(10)
b:push(20)
a:push(300)

print("a all", show(a), show(a.all))
print("b all", show(b), show(b.all))

SpecialAccount = Account:new()

function SpecialAccount:withdraw (v)
  if self.balance - v  < self:getLimit() then
    error"insufficient funds"
  end
  self.balance = self.balance - v
end
    
function SpecialAccount:getLimit ()
  return self.limit or 0
end

s = SpecialAccount:new{limit=100,owners={}}

