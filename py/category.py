class MultiDict(dict):
    def __setitem__(self, k, v):
        if callable(v) and hasattr(v, '__annotations__'):
            a = next(iter(v.__annotations__.values()), None)
            if a: setattr(a, k, v)
        super().__setitem__(k, v)

class _M(type):
    @classmethod
    def __prepare__(mcs, name, bases): return MultiDict()

class Category(metaclass=_M): pass

class Klass:
    def __init__(self, **kw):
        for k, v in self.__class__._defaults.items():
            setattr(self, k, kw.get(k, v))
        self.init()
    def init(self): pass

def make(name, **defaults):
    return type(name, (Klass,), {"_defaults": defaults})

# --- domain code ---

Num   = make("Num",   n=0, mu=0, sd=0, hi=-1e30, lo=1e30)
Sym   = make("Sym",   n=0, most=0, ent=0)
Table = make("Table", cols=None)

class distances(Category):
    def dist(i:Num, a,b): d=abs(a-b); return d/(i.hi-i.lo)
    def dist(i:Sym, a,b): return 0 if a==b else 1
    def dist(i:Table, a,b): return sum(c.dist(x,y) for c,x,y in zip(i.cols,a,b))

class stats(Category):
    def mid(i:Num): return i.mu
    def mid(i:Sym): return i.most
    def div(i:Num): return i.sd
    def div(i:Sym): return i.ent

# --- test ---

n = Num(mu=3.5, sd=1.2, hi=10, lo=0)
s = Sym(most="red", ent=0.9)

print(n.mid())           # 3.5
print(s.mid())           # red
print(n.div())           # 1.2
print(n.dist(3, 7))      # 0.4
print(s.dist("a", "a"))  # 0
print(s.dist("a", "b"))  # 1
