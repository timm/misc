# ez.jl: incremental Bayes  (c) 2026 Tim Menzies  MIT
const HELP = """
ez.jl: incremental Bayes  (c) 2026 Tim Menzies  MIT

Options:
  -k k=1      low frequency Bayes smoothing
  -m m=2      low class frequency smoothing
  -p p=2      Minkowski coefficient
  -d decs=2   decimal places
  -s seed=1   random seed
"""
const Vec, Str, Func = Vector, AbstractString, Function

#-- structs --------------------------------------------------------------------
mutable struct Col; at::Int; txt::String; goal::Bool; n::Float64 end
mutable struct Num; c::Col; mu::Float64; m2::Float64 end
mutable struct Sym; c::Col; has::Dict{Any,Float64} end
mutable struct Cols; names; all; x; y end
mutable struct Data; n::Int; rows; cols; mids end

function Data()            Data(0, [], nothing, nothing) end
function Col(at=0,txt=" ") Col(at, txt, last(txt) != '-', 0.0) end
function Num(at=0,txt=" ") Num(Col(at,txt), 0.0, 0.0) end
function Sym(at=0,txt=" ") Sym(Col(at,txt), Dict()) end
function col(at=0,txt=" ") (isuppercase(first(txt)) ? Num : Sym)(at,txt) end

#-- lib ------------------------------------------------------------------------
function unk(v) v == "?" end
function cell(c, row::Vec) row[c.c.at + 1] end   # 1-indexed
function say(x)
  x isa Float64 ? string(round(x, digits=the[:decs])) : string(x) end

function cast(s::Str)
  s = strip(s)
  s == "true"  && return 1.0
  s == "false" && return 0.0
  n = tryparse(Float64, s)
  n === nothing ? s : n end

function line2row(line::Str)
  txt = strip(splat('#', line)[1])
  isempty(txt) ? nothing : [cast(s) for s in splat(',', txt)] end

function splat(ch::Char, str::Str) [strip(p) for p in split(str, ch)] end

function csv(file::Str, fn::Func)
  open(file) do s
    for line in eachline(s)
      row = line2row(line)
      row !== nothing && fn(row) end end end

function align(m::Vec)
  ss = [[say(v) for v in r] for r in m]
  ws = [maximum(length(r[i]) for r in ss) for i in 1:length(ss[1])]
  for r in ss
    println(join([lpad(v,w) for (v,w) in zip(r,ws)], ", ")) end end

function eachn(lst::Vec, n::Int=30)
  [x for (i,x) in enumerate(lst) if (i-1) % n == 0] end

function pull!(c, x)
  k = findfirst(==(x), c); k !== nothing ? deleteat!(c, k) : c end

#-- cols -----------------------------------------------------------------------
function cols!(names::Vec)
  all = [col(i-1, s) for (i,s) in enumerate(names)]
  endc(c) = last(c.c.txt)
  Cols(names, all, 
       [c for c in all if !in(endc(c), "-+!X")],
       [c for c in all if  in(endc(c), "-+!")]) end

function data!(file::Str)
  d = Data(); csv(file, r -> add(d, r)); d end

#-- update ---------------------------------------------------------------------
function add(i::Sym, v, w::Real=1.0)
  unk(v) && return
  i.c.n += w
  i.has[v] = get(i.has, v, 0.0) + w end

function add(i::Num, v, w::Real=1.0)
  unk(v) && return
  i.c.n += w
  if i.c.n <= 0  i.mu = i.m2 = 0.0
  else d = v - i.mu; i.mu += w * d / i.c.n; i.m2 += w * d * (v - i.mu) end end

function add(i::Data, row::Vec, w::Real=1.0)
  if i.cols === nothing  i.cols = cols!(row)
  else
    i.mids = nothing; i.n += w
    [add(c,cell(c,row),w) for c in i.cols.all] 
    (w > 0 ? push! : pull!)(i.rows, row) end
  row end

function sub(i, v) add(i, v, -1.0) end

#-- query ----------------------------------------------------------------------
function mid(i::Num) i.mu end
function mid(i::Sym) argmax(i.has) end
function mid(i::Data)
  i.mids===nothing ? (i.mids = [mid(c) for c in i.cols.all]) : i.mids end

function spread(i::Num) 
  i.c.n < 2 ? 0.0 : sqrt(max(0.0, i.m2) / (i.c.n - 1)) end
function spread(i::Sym)
  -sum(let p=v/i.c.n; p*log2(p) end for v in values(i.has) if v > 0; init=0.0) end

function z(i::Num, v) clamp((v - i.mu) / (spread(i) + 1e-30), -3.0, 3.0) end

function norm(i::Sym, v) v end
function norm(i::Num, v) unk(v) ? v : 1.0 / (1.0 + exp(-1.7 * z(i,v))) end

#-- distance -------------------------------------------------------------------
function minkowski(vals)
  (sum(x^the[:p] for x in vals) / length(vals)) ^ (1/the[:p]) end

function disty(i::Data, r::Vec)
  minkowski(norm(y, cell(y,r)) - y.c.goal for y in i.cols.y) end

function distx(i::Sym, u, v)
  unk(u) && unk(v) ? 1.0 : (u == v ? 0.0 : 1.0) end

function distx(i::Num, u, v)
  unk(u) && unk(v) && return 1.0
  nu, nv = norm(i,u), norm(i,v)
  unk(nu) && (nu = nv > 0.5 ? 0.0 : 1.0)
  unk(nv) && (nv = nu > 0.5 ? 0.0 : 1.0)
  abs(nu - nv) end

function distx(i::Data, r1::Vec, r2::Vec)
  minkowski(distx(x, cell(x,r1), cell(x,r2)) for x in i.cols.x) end
 
function nearest(d::Data, r::Vec, rows::Vec) first(sortx(d,r,rows)) end
function furthest(d::Data, r::Vec, rows::Vec) last(sortx(d,r,rows)) end
function sortx(d::Data, r::Vec, rows::Vec) 
  sort(rows, by=r2 -> distx(d,r,r2)) end

function pick(i::Sym, v=nothing)
  n = rand() * sum(values(i.has))
  for (k,v) in i.has  (n -= v) <= 0 && return k end end

function pick(i::Num, v=nothing)
  (unk(v) ? i.mu : v) + 2*spread(i)*(sum(rand() for _ in 1:3) - 1.5) end

#-- bayes ----------------------------------------------------------------------
function like(i::Num, v, prior::Real)
  var = spread(i)^2 + 1e-30
  exp(-(v - i.mu)^2 / (2var)) / sqrt(2π * var) end

function like(i::Sym, v, prior::Real)
  max(1e-32, (get(i.has,v,0.0) + the[:k]*prior) / (i.c.n + the[:k])) end

function prior(i, n_all::Int, n_h::Int) 
  (i.n + the[:m]) / (n_all + the[:m] * n_h) end

function like(i::Data, row::Vec, n_all::Int, n_h::Int)
  p = prior(i, n_all, n_h)
  log(p) + sum(let l = like(x, cell(x,row), p); l > 0 ? log(l) : 0.0 end
               for x in i.cols.x if !unk(cell(x,row))) end

#-- demos ----------------------------------------------------------------------
function eg_the(_=nothing) println(the) end

function eg_csv(f::Str)
  out = []; csv(f, r -> push!(out, r)); align(eachn(out)) end

function eg_data(f::Str)
  d = data!(f); align([d.cols.names; [mid(d)]; eachn(d.rows)]) end

function eg_disty(f::Str)
  d = data!(f)
  align([d.cols.names; eachn(sort(d.rows, by=r -> disty(d,r)))]) end

function eg_addsub(f::Str)
  d, one, two = data!(f), nothing, nothing; rows = copy(d.rows)
  for r in reverse(rows) sub(d,r); d.n==50 && (one = mid(d)) end
  for r in rows          add(d,r); d.n==50 && (two = mid(d)) end
  @assert all(!(a isa Float64) || abs(a-b) < 1e-5 for (a,b) in zip(one,two))
  println("ok") end

function eg_like(f::Str)
  d = data!(f); n = d.n
  for r in eachn(d.rows) println(say(round(like(d,r,n,1) * 100) / 100)) end end

#-- main -----------------------------------------------------------------------
function cli(args::Vec)
  i = 1
  while i <= length(args)
    k  = lstrip(args[i], '-'); i += 1
    fn = Symbol("eg_" * k)
    if isdefined(Main, fn)
      arg = i <= length(args) ? cast(args[i]) : nothing
      getfield(Main, fn)(arg); i += 1 end end end

the = Dict(Symbol(k) => cast(v) for (k,v) in eachmatch(r"(\w+)=(\S+)", HELP))
cli(ARGS)
