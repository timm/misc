#!/usr/bin/env julia

using Parameters
using Random
using ResumableFunctions

for f in ["it", "lib", "col"]
  println("# ./" * f * ".jl") 
  include("./" * f * ".jl") 
end

@resumable function two()
  for i in 1:10^8
    if iseven(i) @yield i end end end

onex() = sum(x for x in 1:10^8 if iseven(x))
twox() = sum(x for x in two())

@time onex()
@time twox()
print(onex() == twox())

for ln in eachline("nb.jl")
        println("$(length(ln)), $(ln)")
end

it = It()
Random.seed!(it.seed)

say(Some(w=2))
