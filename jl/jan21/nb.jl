#!/usr/bin/env julia
module NB
using Test
using Random
using Parameters
using ResumableFunctions

no = nothing

want(x) = begin println("# ",x); 
                include("./"*x*".jl") end

want("it")
it=It()

Random.seed!(it.seed)

want("lib")
want("col")
want("tests")

go() = want("nb")
ok()
print(typeof(thing("true")))
# say(Some(w=2))
end
