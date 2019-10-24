using Parameters 

@with_kw mutable struct Cols
    x = (all  = Any[], nums = Any[], syms = Any[])
    y = (all  = Any[], nums = Any[], syms = Any[],
         goals= Any[], klass= nothing)
    all  = Any[] 
    nums = Any[] 
    syms = Any[]
end

z = Cols(y=(goals=[22]))
print(z.y)
w = Cols()
print(w.x.all)
show(nothing)
