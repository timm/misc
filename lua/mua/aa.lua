-- vim: filetype=lua:ts=2:sw=2:sts=2:et:

local lib={}

local function update (t,...)
  for i = 1,select('#',...) do
  for k,v in pairs(select(i,...)) do t[k] = v end end
  return t end

local function import(t,...)
  local other
  t = t or _ENV or getfenv(2)
  local libs = {}
  if select('#',...)==0 then libs[1] = ml
  else for i = 1,select('#',...) do
      local lib = select(i,...)
      if type(lib) == 'string' then
        local value = _G[lib]
        if not value then -- lazy require!
          value = require (lib)
          lib = lib:match '[%w_]+$' end
        lib = {[lib]=value} 
      end
      libs[i] = lib end end
  return update(t,table.unpack(libs)) end

function lib.class(base)
  local klass, base_ctor = {}
  if base then
    import(klass,base)
    klass._base = base
    base_ctor = rawget(base,'_init')
  end
  klass.__index = klass
  klass._class = class
  klass.classof = function(obj)
    local m = getmetatable(obj) -- an object created by class() ?
    if not m or not m._class then return false end
    while m do -- follow the inheritance chain --
      if m == klass then return true end
      m = rawget(m,'_base') 
    end
    return false
  end
  setmetatable(klass,{
      __call = function(klass,...)
        local obj = setmetatable({},klass)
        if rawget(klass,'_init') then
          klass.super = base_ctor
          local res = klass._init(obj,...) 
          if res then obj = setmetatable(res,klass) end
        elseif base_ctor then base_ctor(obj,...) end
        return obj end })
  return klass end

function asserteq (v1,v2)
  if v1 ~= v2 then
    error("failed\nA "..tostring(v1).."\nB "..tostring(v2),2) end end

class=lib.class

local C = class()

--- conventional name for constructor --
function C:_init (name) self.name = name end

-- can define metamethods as well as plain methods
function C:__tostring () return 'name '..self.name end

function C:__eq (other) return self.name == other.name end

c = C('Jones')
assert(tostring(c) == 'name Jones')

-- inherited classes inherit constructors and metamethods
D = class(C)

d = C('Jane')
assert(tostring(d) == 'name Jane')
assert(d == C 'Jane')

-- if you do have a constructor, call the base constructor explicitly
E = class(D)

function E:_init (name,nick)
  self:super(name)
  self.nick = nick end

-- call methods of base class explicitly
-- (you can also use `self._class`)

function E:__tostring () 
  return D.__tostring(self)..' nick '..self.nick end

asserteq(tostring(E('Jones','jj')),'name Jones nick jj')

print(E('Jones','jj'))
