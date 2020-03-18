function show(i,  str,sep)
  str,sep = "{",""
  if i then
    for k,v in pairs(i) do
      if type(v) ~= "function" then
        str = str..sep..tostring(k)..":"..tostring(v)
        sep = ", " end end end
  return str .. "}"
end

Object = {}
Account = {}

function isa(self,o)
  self.__index = self
  self.__tostring = show
  return setmetatable(o, self)
end

function Object:new(o)
    return isa(self,o or {})
end

function Account:xx(b)
  print( tostring(b) .. "111" )
end

function Account:new(o)
  o = isa(self, o or Object:new(o))
  o.balance = o.balance or 1000
  o.all = {}
  return o
end

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

a=Account:new{balance=500000} --balance=100000}
b=Account:new()
b:xx()
a:deposit(100)
b:deposit(200)
b:push(10)
b:push(20)
a:push(300)

print("a all", show(a), show(a.all))
print("b all", show(b), show(b.all))
