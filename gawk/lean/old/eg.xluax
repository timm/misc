function l.try(s, settings,fun,       b4,oops)
  b4 = l.copy(settings)
  math.randomseed(settings.seed or 1234567890)
  io.write("🔷 ".. s.." ")
  oops= fun()==false 
  for k,v in pairs(b4) do settings[k]= v end
  if   oops
  then print(" ❌ FAIL"); return true
  else print("✅ PASS");  return false end  end

function l.run(settings,eg)
  l.cli(settings)
  for _,com in pairs(arg) do 
    if eg[com] then try(com, settings, eg[com]) end end 
  l.rogues() end

function l.runall(settings, eg,      oops)
  oops = -1 -- we have one test that deliberately fails
  for k,fun in l.items(eg) do
    if k~="all" then 
      if try(k,settings,fun) then oops = oops + 1 end end end
  l.rogues()
  os.exit(oops) end

function eg.all() l.runall(the,eg) end 

