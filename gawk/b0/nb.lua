local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end
local the={k=1,m=2}

local Num,Sym,Data

function Num(txt,at) return {at=at or 0, txt=txt or "", n=0, mu=0,m2=0,sd=0} end
function Sym(txt,at) return {at=at or 0, txt=txt or "", n=0, has={},most=0,mode=nil} end
function Data()      return {rows={}, cols=nil} end

-------------------------------------------------------------------------------
local main,head,data,datum,csv

function main(f,     data1,GO) 
  data1,GO = Data(),head
  for row in csv(f) do
    GO(data1,row)
    GO=data end
  return data1 end

function head(data1,row,      all,x,y,col)
  all,x,y = {},{},nil
  for at,s in pairs(row) do
    col = (s:find"^[A-Z]" and Num or Sym)(s,at)
    all[1+#all] = col
    if s:find"!" then y=col else x[1+#x] = col  end end
  return {all=all,y=y,x=x} end
     
function data(data1,row)
  data1.rows[1+#data1.rows] = row
  for at,x in pairs(row) do
    if x ~="?" then
      datum(data1.cols.all[at],x) end end end

function datum(col,x,     d)
  col.n = col.n + 1
  if col.has then
    col.has[x] = 1 + (col.has[x] or 0)
    if col.has[x] > col.most then col.most, col.mode=col.has[x], x end
  else 
    d = x - col.mu
    col.mu = col.mu + d/col.n
    col.m2 = col.m2 + d*(x - col.mu)
    col.sd = col.n < 2 and 0 or (col.m2 / (col.n - 1))^0.5 end end

-------------------------------------------------------------------------------
local coerce

function coerce(s,     FUN,TRIM) 
  TRIM = function(s) return s:match"^%s*(.-)%s*$" end
  FUN  = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or FUN(TRIM(s)) end

function csv(file,     src) 
  if file and file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if s 
    then t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=coerce(s1); return t end
    else if src then io.close(src) end end end end

function o(x,        t,FMT,NUM,LIST.DICT) 
  FMT  = string.format
  NUM  = function() return x//1 == x and tostring(x) or FMT("%.3g",x) end
  LIST = function() for k,v in pairs(x) do t[k]    = o(v) end end
  DICT = function() for k,v in pairs(x) do t[1+#t] = FMT(":%s %s",k, o(v)) end end
  if type(x) == "number" then return NUM() end 
  if type(x) ~= "table"  then return tostring(x) end
  t = {}
  if #x>0 then LIST() else DICT(); table.sort(t) end
  return "(" .. table.concat(t ," ") .. ")" end

function eg_csv(f)
  for row in csv(f) do o(row) end end
-------------------------------------------------------------------------------
for j,s in pairs(arg) do
  s=s:gsub("^--","eg_")
  if _ENV[s] then _ENV[s](arg[j+1]) end

