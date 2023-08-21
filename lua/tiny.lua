local b4={}; for k,_ in pairs(_ENV) do b4[k] = k end
local fmt,o,oo,plus,sort

fmt=string.format

function of(i,f,...) return f.of[f](i,...) end
function sort(t,fun) table.sort(t,fun); return t end
function oo(t) print(o(t)); return t end

function o(t,     u)
  u={}
  if type(t)=="function" then return "FUN()" end
  if type(t)~="table" then return tostring(t) end
  for k,v in pairs(t) do if not k:find"^_" then 
     u[#u+1] = #t==0 and fmt(":%s %s",k,o(v)) or o(v) end end
  return "{"..table.concat(sort(u)," ").."}" end

local l,COL,SYM = {},{is="COL"},{is="SYM"}
function COL.new(n,s)
  s=s or ""
  return {of=COL, at=n or 0, txt=s or "", n=0, w=s:find"-$" and -1 or 1} end

function SYM.new(...)
   local i=COL.new(...)
   i.of,i.mode = SYM,2
   return i end

oo(SYM.new(0,"adas") )
