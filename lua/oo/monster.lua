monster = {}

function show(i,  str,sep)
  str,sep = "{",""
  for k,v in pairs(i) do 
    str = str..sep..tostring(k)..":"..tostring(v)
    sep = ", " 
  end
  return str .. "}"
end

function meta(t,o) 
  m=setmetatable(t,o)
  o.__index = o
  return t
end

function monster.new(name)
  return meta({
    name  = name,
    stats = {power = 10, agility = 10, endurance = 10, filters = {}}},
    monster)
end

function monster:shout()
    print('Aaaaaaa! My name is ' .. self.name .. '!')
end

m1 = monster.new('Katla')
m2 = monster.new('Katla')
m2.stats.filters[#m2.stats.filters + 1] = 10
m2.stats.filters[#m2.stats.filters + 1] = 20
m1:shout()
print("m2",show(m2.stats.filters))
print("m1",show(m1.stats.filters))


