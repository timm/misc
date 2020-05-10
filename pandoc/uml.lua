function one2(...)  return true end
function many2one(...)  return true end
function many2many(...)  return true end
function any(...)  return true end
function method(...) return true end
function explode(...) return true end
function open(...) return true end
function are(...) return true end
function void(...) return true end
function boolean(...) return true end

the={}

the.car = {has={
            color       = are("1","2"),
	    horosepower = are(10,2000)}
	   uses = {
	     wheels     = one2(4, the.wheel),
	     doors      = one2(4, the.door)
	   },
	   does={
	    explode= heat
	    open = boolean}}}
