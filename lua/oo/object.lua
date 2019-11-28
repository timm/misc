-- vim: nospell:sta:et:sw=2:ts=2:sts=2

local Object={}

local function same(x) return x end
local function copy(t)        return type(t) ~= 'table' and t or collect(t,copy) end
local function shallowCopy(t) return map(t,same) end

function Object:new(o)
  o = o or {}
  setmetatable(o,self)
  self.__index = self
  return o
end

return Object
