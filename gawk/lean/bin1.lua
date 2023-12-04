function 
function cells(s1,    t)
  t={}; for s2 in s1:gmatch("([^,]+)") do t[1+#t]=coerce(s2) end; 
  return t end

function csv(fun,src,     s,n)
  src = io.input(src)
  n,s = -1,io.read(src)
  while s do
    n=n+1
    fun(n,cells(s))
    s = io.read(src)  end end
io.close(src)
