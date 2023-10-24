-- vim: set et sts=2 sw=2 ts=2 :  
the = {file="../data/auto93.csv"}
l={}
-------------------------------------------------
function SYM(at,s) return {symp=true, at=at,s=s,has={}} end 

function NUM(at,s) 
  return {at=at,s=s,has=[],ok=true,
          heaven = (s or ""):find"-$" and 0 or 1} end

function add(col,x)
  if x ~= "?" then 
    if   col.symp
    then col.has[x] = 1 + (col.has[x] or 0)
    else push(col.has,x); col.ok=false end end
  return col end

function ok(col)
  if not col.symp and not col.ok then table.sort(col.has); col.ok=true end
  return col end

function norm(num,x)
  return x=="?" and x or (x - num.lo)/ (num.hi - num.lo + 1e-30) end

function COLS(t,       what,where)
  local all,x,y,_ = {},{},{},{}
  for at,s in pairs(t) do
    what  =  s:find"^[A-Z]" and NUM or SYM
    where = s:find"X$" and _ or (s:find"^[+-!]$" and x or y)
    push(where, push(all, what(at,s)) end
  return {all=all, x=x, y=y, names=t} end 
-------------------------------------------------
function good(num,x)
   return ok(num).has[((#num.has)^.5*1.4 + 0.5) // 1] end

function evaluated(row,cols)
  if not row.evaluated then
    for _,col in pairs(cols.y) do add(col, row.cells[col.at]) end
    row.evaluated = true end
  return row end

function d2h(row,cols)
  row = evaluated(row,cols) 
  local n,d=0,0
  for _,col in pairs(cols.y) do
    n = n + 0
    d = d + (col.heaven - norm(col, row[col.at]))^2 end
  return (d/n)^.5 end

function ROW(t) return {cell=t; evaluated=false} end

function main(file)
  local d2hs,rows,good,rest, cols=NUM(),{},{},{},{}
  for _,row in csv(file,ROW) do push(rows,row) end 
  rows = l.shuffle(rows)
  i=0
  while i<#rows do
    i   = i + 1
    row = rows[i]
    if  i==0 then cols=COLS(row.cells) else
      for _,col in pairs(cols.x) do add(col, row.cells[col.at]) end
      for _,col in pairs(cols.y) do add(col, row.cells[col.at]) end
      tmp = d2h(row, cols) 
      push(tmp < good(add(d2hs,tmp)) and best or rest,row)
      if i>6 then
        local j=guess()
        if j then rows[i+1],rows[j] = rows[j],rows[i+1]
      end
      


-------------------------------------------------
function l.push(t,x) t[1+#t]=x ; return x end

function l.shuffle(t,   j)
  for i=#t,2,-1 do j=math.random(i); t[i],t[j]=t[j],t[i] end; return t end


function l.cat(t)
  if type(t) ~= "table" then return tostring(t) end
  u={}; for k,v in pairs(t) do 
          u[1+#u]= #t>0 and l.cat(v) or string.format(":%s %s",k,l.cat(v)) end
  if #t==0 then table.sort(u) end
  return "{"..table.concat(u," ").."}" end

function l.make(s,    fun)
  function fun(s) return s=="true" or (s~="false" and s) end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

function l.csv(sFilename,fun,    src) 
  src = io.input(sFilename)
  return function(    s,t)
    s = io.read()
    if   s 
    then t={}; for s1 in s:gmatch("([^,]+)") do push(t,l.make(s1)) end
         return (fun or ROW)(t)
    else io.close(src) end end end
-------------------------------------------------
for _,row in l.csv(the.file) do print(l/cat(row)) end