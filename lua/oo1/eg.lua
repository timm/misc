--- classes ------
class=require("ml").class 

local _id = 0
function id() _id = _id+1; return _id end

Obj = class()

--- conventional name for constructor --
function Obj:_init (name)
    self.id = id()
    self.name = name
end

-- can define metamethods as well as plain methods
function Obj:__tostring ()
    return 'name '..self.name
end

function Obj:__eq (other) return self.id == other.id end

c = Obj('Jones')
print(10,tostring(c) == 'name Jones')

-- inherited classes inherit constructors and metamethods
D = class(Obj)

d = Obj('Jane')
print(20,tostring(d) == 'name Jane')
print(30,d == Obj 'Jane')

-- if you do have a constructor, call the base constructor explicitly
E = class(D)

function E:_init (name,nick)
    self:super(name)
    self.nick = nick or '*'
end

-- call methods of base class explicitly
-- (you can also use `self._class`)

function E:__tostring ()
    return D.__tostring(self)..' nick '..self.nick
end

e=E(12)
print(40,e)
