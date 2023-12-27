local function as(s) local t={a=s}; t.__index=t; return t end
one=setmetatable

local Person=as"Person"

function Person.new(name,dob)
  return one(Person,{name=name,dob=dob}) end

print(Person.new("tim",23).a)
