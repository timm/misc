local the = {
  train="../../data/auto93.csv",
  seed=1234567891,
  max=256}

math.randomseed(the.seed)

local push,colAdd,dataAdd,adds
--------------------------------------------------------------------------
local Num,Sym,Cols,Data

function Num(s,at)   return {n=0, txt=s, at=at, has={}, max=128, ok=false} end
function Sym(s,at)   return {n=0, txt=s, at=at, has={}, mode=nil, most=0} end
function Data(names) return {cols=Cols(names), rows={}} end

function Cols(names)
  local i = {names=names, all={}, x={}, y={}, klass=nil}
  for at,s in pairs(i.names) do
    local col = push(i.all, (s:find"^[A-Z]" and Num or Sym)(s,at))
    if not s:find"X$" then
      if s:find"!$" then klass = col end
      push(s:find"[!+-]$" and i.y or i.x, col) end end
  return i end

--------------------------------------------------------------------------
function colAdds(t,it)
   for _,x in pairs(t) do colAdd(x,it) end
   return it end

function colAdd(x,it,      SYM,NUM)
  SYM= function(a)
         a[x] = (a[x] or 0) + 1 
         if a[x] > it.most then it.most,it.mode = a[x],x end end
  NUM= function(a,     pos)
         if #a < it.max then pos=#a+1 
         elseif math.random() < #a/n then pos=math.random(#a) end
         if pos then
           a[pos] = x
           it.ok = false end end 
  if x ~= "?" then
    it = it or type(x)=="number" and Num() or Sym()
    it.n = it.n + 1
    (it.mode and SYM or NUM)(it.has) end 
  return it end  

function dataAdd(row,it)
  if it then
    push(it.rows,row)
    for _,col in pairs(it.cols.all) do colAdd(row[col.at], col) end
  else 
    it = Data(row) end
  return it end

function dataAdds(t, it)   
  for _,x in pairs(t) do it = dataAdd(x,it) end 
  return it end

add,adds=addCol,addsCol

--------------------------------------------------------------------------
local clone,div,hi,lo,mid,norm,ok,per

function ok(i)          if not i.ok then table.sort(i.has); i.ok=true end; return i end
function norm(i,x)      return x=="?" and x or (x - lo(i))/(hi(i) - lo(i)) end
function clone(i, rows) return addRows(rows or {}, Data(i.cols.names)) end
function mid(i)         return i.mode and i.mode or per(i,0.5) end
function per(i,n)       return ok(i).has[#i.has*n//1] end
function hi(i)          return ok(i).has[#i.has] end
function lo(i)          return ok(i).has[1] end
function div(i,      e) 
  if i.mode 
  then e=0; for _,n in pairs(i.has) do e = e - n/i.n*math.log(n/i.n,2) end; return e 
  else return (per(i,0.9) - per(i,0.1)) / 2.58 end end

--------------------------------------------------------------------------
local coerce,words,csv,o

function coerce(s)
  return s=="?" and s or math.tointeger(s) or tonumber(s) or 
         s=="true" and true or s ~= "false" and s end

function words(s,     t)
  if s then
    t={}
    for s1 in s:gmatch"([^,]+)" do t[1+#t] = coerce(s:match"^%s*(.-)%s*$") end
    return t end end

function csv()
  return function(         line)
    line = io.read()
    if line then return words(line) end end end

function o(t,     u,fmt)
  fmt=string.format
  if type(t) == "number" then 
    return t//1 == t and tostring(t//1) or fmt("%.3f",t) end
  if type(t) ~= "table"  then return tostring(t) end
  u={}; for k,v in pairs(t) do u[1+#u] = #t==0 and fmt(":%s %s",k, o(v)) or o(v) end
  if #t==0 then table.sort(u) end
  return "{" .. table.concat(u," ") .. "}" end
