local the,o,new,csv,coerce = {
  train="../../data/auto93.csv" }

function coerce(s,     FUN) 
  FUN = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or FUN(s:match"^%s*(.-)%s*$") end

function csv(      s,t)
  return function()
    s = io.read()
    if s then t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=coerce(s1) end; return t end end end

function o(t,     u)
  if type(t) ~= "table"  then return tostring(t) end
  u={}; for k,v in pairs(t) do u[1+#u] = #t==0 and string.format(":%s %s",k, o(v)) or o(v) end
  if #t==0 then table.sort(u) end
  return "(" .. table.concat(u," ") .. ")" end

function new(kl,t)
  kl.__index=kl; kl.__tostring = o; return setmetatable(t,kl) end

print(o{a=1,l=200,b=20})

Cols,Data,Num,Sym = {},{},{},{}

function Num:new(txt,at) 
  return new(Num, {at=at or 0, txt=txt or "", n=0, 
                   mu=0, m2=0, sd=0, lo = math.huge, hi= -math.huge})

function Sym:new(txt,at) 
  return new(Sym, {at=at or 0, txt=txt or "", n=0, has={},most=0,mode=nil}) end

function Cols:new(names,    col)
  self = {names=names,all={}, y={}, x={}, klass=nil}
  for at,s in pairs(self.names) do
    col = push(self.all, (s:find"^[A-Z]" and Num or Sym):new(s,at))
    if s:find"!$" then self.klass = col end
    push(s:find"[!+-]$" and self.y or self.x, col) end 
  return self end

function Data:new(name) return {cols=Cols:new(names), rows={}} end

-- function Data:add(row)
--   a[1+#a] = row 
--   for _,col in pairs(self.cols.all) do col:add(row[col.at]) end 
--   return self end
--   
