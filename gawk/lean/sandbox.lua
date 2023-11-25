function byte2bin(n)
  local t = {}
  for i=7,0,-1 do
    t[#t+1] = math.floor(n / 2^i)
    n = n % 2^i
  end
  return table.concat(t)
end

function count(n)
  local count = 0
  while n ~= 0 do
    n = n & (n - 1)
    count = count + 1
  end
  return count
end


for i=1,100 do
  x= math.random(2^32 - 1)
  print(x, byte2bin(x), count(x)) end

