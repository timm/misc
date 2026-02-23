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
the = Dict(Symbol(k) => cast(v)
           for (k,v) in eachmatch(r"(\w+)=(\S+)", HELP))

#-- structs --------------------------------------------------------------------
mutable struct Col; at::Int; txt::String; goal::Bool; n::Float64 end
mutable struct Num; c::Col; mu::Float64; m2::Float64 end
mutable struct Sym; c::Col; has::Dict{Any,Float64} end
mutable struct Cols; names; all; x; y end
mutable struct Data; n::Int; rows; cols; mids end

Col(at,txt) = Col(at, txt, last(txt) != '-', 0.0)
Num(at,txt) = Num(Col(at,txt), 0.0, 0.0)
Sym(at,txt) = Sym(Col(at,txt), Dict())
col(at,txt) = isuppercase(first(txt)) ? Num(at,txt) : Sym(at,txt)
Data()      = Data(0, [], nothing, nothing)

#-- lib ------------------------------------------------------------------------
unk(v)       = v == "?"
cell(c,row)  = row[c.c.at + 1]   # 1-indexed
say(x)       = x isa Float64 ? round(x, digits=the.decs) |> string : string(x)

function cast(s)
  s = strip(s)
  s == "true"  && return 1.0
  s == "false" && return 0.0
  n = tryparse(Float64, s)
  n === nothing ? s : n  end

function line2row(line)
  txt = strip(splat('#', line)[1])
  isempty(txt) ? nothing : [cast(s) for s in splat(',', txt)]  end

splat(ch, str) = [strip(p) for p in split(str, ch)]

function csv(file, fn)
  open(file) do s
    for line in eachline(s)
      row = line2row(line)
      row !== nothing && fn(row) end end end

function align(m)
  ss = [[say(v) for v in r] for r in m]
  ws = [maximum(length(r[i]) for r in ss) for i in 1:length(ss[1])]
  for r in ss
    println(join([lpad(v,w) for (v,w) in zip(r,ws)], ", "))    end  end

eachn(lst, n=30) = [x for (i,x) in enumerate(lst) if (i-1) % n == 0]

#-- cols -----------------------------------------------------------------------
function cols!(names)
  all = [col(i-1, s) for (i,s) in enumerate(names)]
  endc(c)  = last(c.c.txt)
  Cols(names, all,
       [c for c in all if !in(endc(c), "-+!X")],
       [c for c in all if  in(endc(c), "-+!")])  end

function data!(file)
  d = Data()
  csv(file, r -> add!(d, r))
  d  end

#-- update ---------------------------------------------------------------------
function add!(i::Sym, v, w=1.0)
  unk(v) && return
  i.c.n += w
  i.has[v] = get(i.has, v, 0.0) + w  end

function add!(i::Num, v, w=1.0)
  unk(v) && return
  i.c.n += w
  if i.c.n <= 0  i.mu = i.m2 = 0.0
  else d = v - i.mu
       i.mu += w * d / i.c.n
       i.m2 += w * d * (v - i.mu)    end  end

function add!(i::Data, row, w=1.0)
  if i.cols === nothing  i.cols = cols!(row)
  else
    i.mids = nothing
    i.n   += w
    for c in i.cols.all  add!(c, cell(c, row), w) end
    w > 0 ? push!(i.rows, row) :
            filter!(r -> r != row, i.rows)    end
  row end

sub!(i, v) = add!(i, v, -1.0)

#-- query ----------------------------------------------------------------------
mid(i::Num) = i.mu
mid(i::Sym) = argmax(i.has)

mids(i) = i.mids === nothing ?
           (i.mids = [mid(c) for c in i.cols.all]) : i.mids

spread(i::Num) = i.c.n < 2 ? 0.0 : sqrt(max(0.0, i.m2) / (i.c.n - 1))
spread(i::Sym) = -sum(let p=v/i.c.n; p*log2(p) end
                      for v in values(i.has) if v > 0; init=0.0)

z(i,v)    = clamp((v - i.mu) / (spread(i) + 1e-30), -3.0, 3.0)
norm(i::Sym, v) = v
norm(i::Num, v) = unk(v) ? v : 1.0 / (1.0 + exp(-1.7 * z(i,v)))

#-- distance -------------------------------------------------------------------
function minkowski(vals)
  p = the.p
  isempty(vals) && return 0.0
  (sum(x^p for x in vals) / length(vals)) ^ (1/p)  end

function disty(i, r)
  minkowski([let v = norm(y, cell(y,r))
               (unk(v) ? 0.0 : v) - (y.c.goal ? 1.0 : 0.0)
             end for y in i.cols.y])  end

function aha(i, u, v)
  unk(u) && unk(v) && return 1.0
  i isa Sym && return u == v ? 0.0 : 1.0
  nu, nv = norm(i,u), norm(i,v)
  unk(nu) && (nu = nv > 0.5 ? 0.0 : 1.0)
  unk(nv) && (nv = nu > 0.5 ? 0.0 : 1.0)
  abs(nu - nv)  end

distx(i, r1, r2) =
  minkowski([aha(x, cell(x,r1), cell(x,r2)) for x in i.cols.x])

order(d,r,rows)   = sort(rows, by=r2 -> distx(d,r,r2))
nearest(d,r,rows) = first(order(d,r,rows))
furthest(d,r,rows)= last(order(d,r,rows))

function nearby(i::Sym, v)
  n = rand() * sum(values(i.has))
  for (k,v) in i.has  (n -= v) <= 0 && return k end  end

nearby(i::Num, v) =
  (unk(v) ? i.mu : v) + 2*spread(i)*(sum(rand() for _ in 1:3) - 1.5)

#-- bayes ----------------------------------------------------------------------
function like(i::Num, v, prior)
  var = spread(i)^2 + 1e-30
  exp(-(v - i.mu)^2 / (2var)) / sqrt(2π * var)  end

like(i::Sym, v, prior) =
  max(1e-32, (get(i.has,v,0.0) + the.k*prior) / (i.c.n + the.k))

prior(i, n_all, n_h) = (i.n + the.m) / (n_all + the.m * n_h)

function likes(i, row, n_all, n_h)
  p = prior(i, n_all, n_h)
  log(p) + sum(let l = like(x, cell(x,row), p)
                 l > 0 ? log(l) : 0.0     end
               for x in i.cols.x if !unk(cell(x,row)))  end

#-- demos ----------------------------------------------------------------------
eg_the(_=nothing) = println(the)

function eg_csv(f)
  out = []
  csv(f, r -> push!(out, r))
  align(eachn(out))  end

function eg_data(f)
  d = data!(f)
  align([d.cols.names; [mids(d)]; eachn(d.rows)])  end

function eg_disty(f)
  d = data!(f)
  align([d.cols.names;
         eachn(sort(d.rows, by=r -> disty(d,r)))])  end

function eg_addsub(f)
  d, one, two = data!(f), nothing, nothing
  rows = copy(d.rows)
  for r in reverse(rows)
    sub!(d,r); d.n==50 && (one = mids(d)) end
  for r in rows
    add!(d,r); d.n==50 && (two = mids(d)) end
  @assert all(!(a isa Float64) || abs(a-b) < 1e-5 for (a,b) in zip(one,two))
  println("ok")  end

function eg_bayes(f)
  d, n = data!(f), data!(f).n
  for r in eachn(d.rows)
    println(say(round(likes(d,r,n,1) * 100) / 100))    end  end

#-- main -----------------------------------------------------------------------
function cli(args)
  i = 1
  while i <= length(args)
    k   = lstrip(args[i], '-');  i += 1
    fn  = Symbol("eg_" * k)
    if isdefined(Main, fn)
      arg = i <= length(args) ? cast(args[i]) : nothing
      getfield(Main, fn)(arg);  i += 1      end    end  end

cli(ARGS)
