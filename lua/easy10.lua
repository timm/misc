function ordered(t,  i,keys)
  i,keys = 0,{}
  for key,_ in pairs(t) do keys[#keys+1] = key end
  table.sort(keys)
  return function ()
    if i < #keys then
      i=i+1; return keys[i], t[keys[i]] end end
end

function o(t,    pre) print(oo(t,pre))  end

function oo(t,     s,sep)
  s, sep = '',''
  for k, v in ordered(t) do
    if not (type(k)=='string' and k:match("^_")) then
      v   = type(v) == 'table' and oo(v) or tostring(v) 
      s   = s .. sep .. tostring(k) .. '=' .. v
      sep = ', ' end end 
  return '{' .. s .. '}'
end


math.randomseed(1)
r=math.random
function round(x) return math.floor(x + 0.5) end

Object={}

function Object:subclass(o)
   o = o or {} 
   setmetatable(o,self)  
   self.__index = self
   self.__tostring = oo
   return o
end

function Object:has(t)
  if t then
    for k,v in pairs(t) do
      self[k] = v
    end end
  return self
end

function Object.new(t)
  return t:subclass()
end

Some = Object:subclass()
function Some.new(o)
  o      = Object.new(o or Some)
  o.max  = 32
  o._kept = {}
  o.n    = 0
  return o
end

Log = Object:subclass()
function Log.new(o)
  o      = Object.new(o or Log)
  o.name = ""
  o.pass = "[\\?]"
  o.n    = 0
  o.some = Some.new()
   return o
end
function Log:say(x)
  print("asd",x)
end
Sym = Log:subclass()
function Sym.new(o)
  o        = Log.new(o or Sym)
  o.counts = {}
  o.mode   = nil
  o.most   = 0
   return o
end

Num = Log:subclass()
function Num.new(o)
  o = Log.new(o or Num)
  o.up = -1*10^32
  o.lo = 10^32
  o.mu = 0
  o.m2 = 0
  o.sum = 0
  return o
end

function Num:add(x)
  print("x",x)
  self.sum = self.sum + x
  self.n   = self.n   + 1 
end

function Num:mid()
  return self.sum/ self.n
end

Logs = Object:subclass()
function Logs.new(o)
  o = Object.new(o or Logs)
  o.has  = {}
  o.some = Some.new()
  return o
end

function Some:mid(x)
  table.sort(self._kept)
  return self._kept[  #self._kept // 2 ]
end

function Some:add(x)
  self.n  = self.n + 1
  local k = #self._kept
  if     k < self.max     then self._kept[k+1] =x  
  elseif r() < k / self.n then self._kept[round(r()*k)]= x
  end 
  return x
end

--nu=Num.new()
--for i=1,200 do  nu:add(i) end
--table.sort(s._kept)
--o(s._kept)
--o(Logs.new())


nu=Num.new()
s=Some.new()
for i=1,200 do  
  s:add(i) 
  nu:add(i)
end
print(s:mid())
print(nu:mid())
nu:say(2)
s:say(2)
