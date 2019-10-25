using Parameters 

@with_kw mutable struct Cols
  x  = (all  = [], nums = [], syms = [])
  y = (all  = [], nums = [], syms = [],
           goals= [22], klass= nothing)
  all  = [] 
  nums = [] 
  syms = []
  name=""
  pos=0
end

z=Cols(name="jane", pos=2)
show(z)
#w = Cols()
#dump(w)
#dump(nothing)

open("got.jl") do file
    n=1
    for line in eachline(file)
        words= split(strip(line), "a[ \t]*,a[ \t]*")
        println(words) 
    end
end
