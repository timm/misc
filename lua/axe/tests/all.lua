_assert = assert

function assert(x,y) 
  print("-- " .. (y or ""))
  _assert(x,y)
end

for x in io.stdin:lines() do
  if x:match(".lua$") then
    if x ~= "all.lua" then
      print("---------- " .. x)
      dofile(x) end end end


