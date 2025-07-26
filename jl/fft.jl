using Random, Printf
using Base: @kwdef

@kwdef mutable struct Num
  at::Int = 0
  txt::String = " "
  lo::Float64 = 1e30
  hi::Float64 = -1e30
  mu::Float64 = 0.0
  m2::Float64 = 0.0
  n::Int = 0
  goal::Int = startswith(txt, "-") ? 0 : 1  # We'll fix this below
end

@kwdef mutable struct Sym
  at::Int = 0
  txt::String = ""
  has::Dict{Any, Int} = Dict()
end

@kwdef mutable struct Data
  rows::Vector{Any} = Any[]
  cols = nothing
end

@kwdef mutable struct Cols
  all::Vector{Any} = []
  x::Vector{Any} = []
  y::Vector{Any} = []
  klass = nothing
end

function coerce(x::String, types=[Int, Float64])
  for T in types
    try return parse(T, x) catch end end
  return x == "True" ? true : x == "False" ? false : x end

function add(x::Num | Sym, v)
  v != "?" && add1(x, v)
  return v end

function add1(x::Sym, v) x.has[v] = get(x.has, v, 0) + 1 end

function add1(x::Num, v)
  x.n  += 1
  d     = v - x.mu
  x.mu += d / x.n
  x.m2 += d * (v - x.mu)
  x.lo  = min(x.lo, v)
  x.hi  = max(x.hi, v) end

function add1(x::Data, v)
  if x.cols === nothing x.cols = header(v) else
    push!(x.rows, [add(c, v[c.at]) for c in x.cols.all]) end end

function adds(src, x=nothing)
  it === nothing && (x = Num())
  for v in src; add(x, v) end
  return x end

function norm(x::Num, v)
  return (v - x.lo) / (x.hi - x.lo + 1e-30) end

function header(names::Vector{String})
  cols = Cols()
  for (c, s) in enumerate(names)
    col = isuppercase(s[1]) ? Num(c, s) : Sym(c, s)
    push!(cols.all, col)
    if endswith(s, "X"); continue end
    if endswith(s, "!"); cols.klass = col end
    push!(endswith(s, "!-+") ? cols.y : cols.x, col) end
  return cols end

function readcsv(file::String)
  data = Data()
  for line in eachline(file)
    startswith(line, "#") || isempty(strip(line)) && continue
    row = [coerce(w) for w in split(strip(line), ",")]
    add(data, row) end
  return data end

function distx(data, r1, r2, p=2)
  d = sum(dist1(col, r1[col.at], r2[col.at])^p for col in data.cols.x)
  return (d / length(data.cols.x))^(1/p) end

function disty(data, row, p=2)
  d = sum(abs(norm(c, row[c.at]) - c.goal)^p for c in data.cols.y)
  return (d / length(data.cols.y))^(1/p) end
