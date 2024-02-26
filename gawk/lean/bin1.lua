--<!-- vim: set syntax=lua ts=3 sw=3 et : -->
function push(t,x) t[1+#t]=x; return x end

function cat(t)
  u={}; for k,v in pairs(t) do u[k]=tostring(v) end
  print(table.concat(u),", ") end

function coerce(s,    fun)
  function fun(s)
    if s=="nil" then return nil
    else return s=="true" or (s~="false" and s) end end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

function cells(s1,    t)
  t={}; for s2 in s1:gmatch("([^,]+)") do t[1+#t]=coerce(s2) end; 
  return t end

function csv(fun,fileName,     s,n)
  src = io.input(fileName)
  n,s = -1,io.read(src)
  while s do
    n=n+1
    fun(n,s)
    s = io.read(src)  end 
  io.close(src) end

function line(data1,nr,s) 
  if nr==0 then
    print(s)
    for k,v in pairs(cells(s)) do 
      data1.cols[k] = {}
      if v:find"^[A-Z]" then num[k]={} end end
  else
    data1.rows[nr]={}
    for k,v in pairs(cells(s)) do
      data1.rows[nr][k] = v
      if num[k] then
        if v ~="?" then push(data1.cols[k],v) end end end end end 

functio
data={rows={}, cols={}}
csv(line)
