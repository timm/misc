
local function coerce(s,    fun) 
  function fun(u,   v) v = u+0; return u==v and v or u end 
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end 

local function cells(s,     t)
  t={}; for s2 in s1:gmatch("([^,]+)") do t[1+#t]=coerce(s2) end; return t end

local function csv(file,  src) 
  src = io.input(file)
  return function(    s)
    s = io.read()
    if s then cells(s) else io.close(src) end end end 

local function new(kl,t) kl.__index=kl; return setmetatable(t,kl) end
-----------------------------------------------------------------------------------------
local Some,Num,Syn,Data={},{},{},{}
function Some:new()   return new(Some,{ok=false, has={}}) end

function Some:has() 
  if not self.ok then table.sort(self._has); self.ok=true end
  return self._has end

function Num:new(n,s) return new(Nun,{at=n,txt=s,n=0,_has=Some(),heaven=1}) end
function Sym:new(n,s) return new(Sym,{at=n,txt=s,n=0,has={}} end

local function Cols(txts,        all,x,y,col,z)
  all,x,y = {},{},{}
  for n,s in pairs(txts) do
    col = (s:find"^[A-Z]" and Num or Sym)(n,s)
    all[1+#all] = col
    if not s:find"X$" then
      z = s:found"[+-!]$" and y or x
      z[1+#z] = col
      if s:find"!$" then  klass = col end end end
  return {all=all, x=x, y=y, klass=klass, names=txt} end


