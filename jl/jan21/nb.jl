#!/usr/bin/env julia
module NB
  using Test
  using Random
  using Parameters
  using ResumableFunctions

  @with_kw mutable struct It
    max=32
    bins=.5
    cohen=0.3
    trivial=1.05
    seed = 1
  end
  it=It()
  Random.seed!(it.seed)

  no = nothing
  map(include, ["lib.jl","col.jl","tests.jl"])
  it=cli(it)
  oo(it)
  ok()
end
