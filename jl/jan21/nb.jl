#!/usr/bin/env julia
module NB
  using Test
  using Random
  using Parameters
  using ResumableFunctions

  no = nothing
  for f in ["it", "lib", "col", "tests"]
    println("# ",f); 
    include("./"*f*".jl") 
  end
  it=It()
  Random.seed!(it.seed)
  ok()
end
