#!/usr/bin/env julia

using Parameters
using Random
using ResumableFunctions

function go1(files...)
  go1(x) = begin println("# ",x); include(x) end
  [go1("./"*x*".jl") for x in files]; 1 end

go() = go1("it", "lib", "col", "tests")

go()

it = It()
Random.seed!(it.seed)

# @resumable function two()
#   for i in 1:10^8
#     if iseven(i) @yield i end end end
#
# onex() = sum(x for x in 1:10^8 if iseven(x))
# twox() = sum(x for x in two())
#
# @time onex()
# @time twox()
# #print(onex() == twox())
#
# #for ln in eachline("nb.jl") println("$(length(ln)), $(ln)") end
#
#
# say(Some(w=2))
1
