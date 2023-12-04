local l={}

function l.per(t,p) return t[(p*#t)//1] end
function l.mid(t)   return l.per(t,.5) end
function l.div(t)   return (l.per(t,.9) - l.per(t,.1))/2.56 end

function l.push(t,x) t[1+#t]=x; return x end

function l.cat(t,     u)
  u={}; for k,v in pairs(t) do u[k]=tostring(v) end
  print() table.concat(u,", ")) end

function l.coerce(s,    fun)
  function fun(s)
    if s=="nil" then return nil
    else return s=="true" or (s~="false" and s) end end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

function l.cells(s1,    t)
  t={}; for s2 in s1:gmatch("([^,]+)") do t[1+#t]=l.coerce(s2) end; 
  return t end

function l.csv(src,fun,    line,nr)
  src =  src=="" and io.input() or io.input(src)
  nr,line = -1,io.read()
  while line do
    nr=nr+1
    fun(nr,line)
    line = io.read() end
  io.close(src) end

function l.cli(t) 
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        v= ((v=="false" and "true") or (v=="true" and "false") or arg[n+1])
        t[k] = l.coerce(v) end end end
  return t end
    
return l