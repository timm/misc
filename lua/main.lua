function loadz(f,    s,code,fp)
  s,sep,code = "","",false
  fp = io.input(f)
  for l in fp:lines() do
    if   l:sub(1,3)=="```"  
    then s = s..sep
         code = not code
    else s = s..sep..(code and "" or "-- ") ..l end
    sep = "\n"
  end
  io.close(fp) 
  return s end

x=load(loadz("f.md"))()
print(type(x))
