one=setmetatable
local function maker(t) return function(_,...) return t.new(...) end end
local function of(s)    local t={a=s}; t.__index=t; return one(t,{__call=maker(t)}) end

local Person=of"Person"

function Person.new(name,dob)
  return one(Person,{name=name,dob=dob}) end

print(Person("tim",23).a)
