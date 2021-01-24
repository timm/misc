function loadz(f)
  sep,ss,s,fp = "",{},"",io.open(f)
  for l in fp:lines() do
    if   l=="" 
    then s=""
    else if #s==0 then ss[#ss+1]=""  end  
         s = s .. l .. sep; sep="\n"
         ss[#ss] = s
    end
  end
  io.close(fp)
  return ss
end

for _,x in pairs(loadz("f.lua")) do
  print("----",x)
end
