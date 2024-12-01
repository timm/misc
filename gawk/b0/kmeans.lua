local the = {
  train="../../data/auto93.csv",
  seed=1234567891,
  max=256}

math.randomseed(the.seed)

local Cols,Data,Num,Sym
local add, adds,row,rows
--------------------------------------------------------------------------

function Num(s,at)   return {n=0, txt=s, at=at, has={}, max=128, ok=false} end
function Sym(s,at)   return {n=0, txt=s, at=at, has={}, mode=nil, most=0} end
function Data(names) return {cols=Cols(names), rows={}} end

function Cols(names,      it,col)
  it = {names=names, all={}, x={}, y={}, klass=nil}
  for at,s in pairs(it.names) do
    col = push(it.all, (s:find"^[A-Z]" and Num or Sym)(s,at))
    if not s:find"X$" then
      if s:find"!$" then klass = col end
      push(s:find"[!+-]$" and it.y or it.x, col) end end
  return it end

--------------------------------------------------------------------------
function add(x,  col1,      SYM,NUM)
  SYM= function(a)
         a[x] = (a[x] or 0) + 1 
         if a[x] > col1.most then col1.most,col1.mode = a[x],x end end
  NUM= function(a,     pos)
         if #a < col1.max then pos=#a+1 
         elseif math.random() < #a/n then pos=math.random(#a) end
         if pos then
           a[pos] = x
           col1.ok = false end end 
  if x ~= "?" then
    col1   = col1 or type(x)=="number" and Num() or Sym()
    col1.n = col1.n + 1
    (col1.mode and SYM or NUM)(col1.has) end 
  return col1 end  

function row(row1,  data1)
  if not data1 then return Data(row1) end
  push(data1.rows, row1)
  for _,col1 in pairs(data1.cols.all) do add(row1[col1.at], col1) end
  return data1 end

function adds(t,  col1) 
  for _,x in pairs(t or {}) do col1=adds(x,col1) end; return col1 end

function rows(t,  data1) 
  for _,row1 in pairs(t or {}) do data1=row(row1,data1) end; return data1 end

function reads(        data1) 
  for _,row1 in csv() do data1=row(row1,data1) end; return data1 end

--------------------------------------------------------------------------
function ok(it)  if not it.ok then table.sort(it.has); it.ok=true end; return it end

function norm(it,x)      return x=="?" and x or (x - lo(it))/(hi(it) - lo(it) + 1E-32) end
function clone(it,rows1) return rows(rows1,  Data(it.cols.names)) end
function mid(it)         return it.mode and it.mode or per(i,0.5) end
function per(it,n)       return ok(it).has[#it.has*n//1] end
function hi(it)          return ok(it).has[#it.has] end
function lo(it)          return ok(it).has[1] end
function div(it,      e) 
  if it.mode 
  then e=0; for _,n in pairs(it.has) do e = e - n/it.n*math.log(n/it.n,2) end; return e 
  else return (per(it,0.9) - per(it,0.1)) / 2.58 end end

function mids(data1,    t={}
  t={}; for _,col in pairs(data1.cols.all) do push(t, mid(col)) end; return t end
------------------------------------------------------------------------------------------
function push(t,x) t[1+#t]=x; return x end

function coerce(s)
  return s=="?" and s or math.tointeger(s) or tonumber(s) or 
         s=="true" and true or s ~= "false" and s end

function cells(s,     t)
  t={}
  for s1 in s:gmatch"([^,]+)" do t[1+#t] = coerce(s:match"^%s*(.-)%s*$") end
  return t end 

function csv()
  return function(    s)
    s = io.read()
    if s then return cells(s) end end end

function o(t,     u,fmt)
  fmt=string.format
  if type(t) == "number" then 
    return t//1 == t and tostring(t//1) or fmt("%.3f",t) end
  if type(t) ~= "table"  then return tostring(t) end
  u={}; for k,v in pairs(t) do u[1+#u] = #t==0 and fmt(":%s %s",k, o(v)) or o(v) end
  if #t==0 then table.sort(u) end
  return "{" .. table.concat(u," ") .. "}" end
