#!/usr/bin/env julia

using Random
include("./it.jl")
include("./lib.jl")
include("./col.jl")

it = It()
Random.seed!(it.seed)
println(rand())
println(2)

say(Some(w=2))
