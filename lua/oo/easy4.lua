Object = {__init = function(o) end}

function Object:new(init)
  -- when creating an instance:
  --  o: the object itself
  -- when creating a class:
  --  o:    table containing shared fields and methods
  --  init: initializer for instances of this class
  o =  {}
  self.__init(o)
  setmetatable(o, self)
  self.__index = self
  if init then
    function o.__init(o)
      self.__init(o)
      init(o)
    end
  end
  return o
end

Account = Object:new(function(o) o.all={}; o.balance=0; end)

function Account:push(v)
  self.all[#self.all+1] = v
end

function Account:deposit(v)
  self.balance = self.balance + v
end

function Account:withdraw (v)
  if v > self.balance then
    error"insufficient funds" end
  self.balance = self.balance - v
end
b
a=Account:new()
b=Account:new()
a:deposit(100)
b:deposit(200)
b:push(10)
b:push(20)
a:push(300)

print("a all", show(a), show(a.all))
print("b all", show(b), show(b.all))


