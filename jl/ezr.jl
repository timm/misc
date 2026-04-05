#!/usr/bin/env julia --compile=min --startup-file=no -O0 
# ez.jl: incremental Bayes  (c) 2026 Tim Menzies  MIT
HELP = """ 
ez.jl: incremental Bayes  (c) 2026 Tim Menzies  MIT

Options:
  -k 1      low frequency Bayes smoothing
  -m 2      low class frequency smoothing
  -p 2      Minkowski coefficient
  --decs 2   decimal places
  --seed 1   random seed
"""
import Random

#-- structs ---------------------------------------------------------
@kwdef mutable struct Col;  at=0; txt=" "; heaven=0; n=0 end
@kwdef mutable struct Num;  col; mu=0.0; m2=0.0 end
@kwdef mutable struct Sym;  col; has end
       mutable struct Cols; strs; all; xs; ys end
@kwdef mutable struct Data; n=0; rows; cols; mids end

Col(i,str)    = Col(i, str, last(str) != '-', 0)
Num(i,str)    = Num(Col(i,str), 0.0, 0.0)
Sym(i,str)    = Sym(Col(i,str), Dict())
newcol(i,str) = (isuppercase(first(str)) ? Num : Sym)(i,str)
Data()        = Data(0, [], nothing, nothing)

#-- lib -------------------------------------------------------------
unk(val)       = val == "?"
cell(col, row) = row[col.col.at + 1]   # 1-indexed
say(val)       = string(val isa Float64 ? round(val, digits=the[:decs]) : val)

function line2row(str)
  txt = strip(splat('#', str)[1])
  isempty(txt) ? nothing : [make(s) for s in splat(',', txt)]  end

splat(ch, str) = [strip(s) for s in split(str, ch)]

function make(str)
  str = strip(str)
  for T in (Int, Float64, Bool)
    n = tryparse(T, str)
    n !== nothing && return n  end
  str  end

the= Dict(Symbol(m[1]) => make(m[2]) for m in eachmatch(r"-(\w+)\s+(\S+)",HELP))

function csv(filename::String, fun)
  open(filename) do s
    for str in eachline(s)
      (row = line2row(str)) !== nothing && fun(row)  end end  end

function align(rows)
  strs = [[say(val) for val in row] for row in rows]
  ws   = [maximum(length(row[n]) for row in strs) for n in 1:length(strs[1])]
  for row in strs
    println(join([lpad(val,w) for (val,w) in zip(row,ws)], ", "))  end  end

eachn(rows, n=30) = [row for (i,row) in enumerate(rows) if (i-1) % n == 0]

#-- cols ------------------------------------------------------------
function cols!(strs)
  all  = [newcol(n-1, str) for (n,str) in enumerate(strs)]
  endc(col) = last(col.col.txt)
  Cols(strs, all,
      [col for col in all if !in(endc(col), "-+!X")],
      [col for col in all if  in(endc(col), "-+!")])  end

function data!(file)
  data = Data()
  csv(file, row -> add!(data, row))
  data end

#-- update ----------------------------------------------------------
function sub!(val1, val2) add!(val1, val2, -1.0) end

function adds!(vals::Vector, summary=Num()) 
  [add!(summary,val) for val in vals]; summary end

function add!(sym::Sym, val, w=1.0)
  unk(val) && return
  sym.col.n += w
  sym.has[val] = w + get(sym.has, val, 0) end

function add!(num::Num, val, w=1.0)
  unk(val) && return
  num.col.n += w
  if num.col.n <= 0  num.mu = num.m2 = 0.0
  else delta  = val - num.mu
       num.mu += w * delta / num.col.n
       num.m2 += w * delta * (val - num.mu)  end  end

function add!(cols::Cols, row, w=1.0)
  [add!(col, cell(col,row)) for col in cols.all] end

function add!(data::Data, row, w=1.0)
  if data.cols === nothing  data.cols = cols!(row)
  else
    data.mids  = nothing
    data.n    += w
    add!(data.cols, row, w)
    if w > 0  push!(data.rows, row) end end
    #if w<0 decrement data, sub!(data.cols,row) and remove from data.rows
  row end


#-- query -----------------------------------------------------------
mid(num::Num) = num.mu
mid(sym::Sym) = argmax(sym.has)

mids(data) = data.mids === nothing ?
             (data.mids = [mid(col) for col in data.cols.all]) : data.mids

spread(num::Num) = num.col.n < 2 ? 0.0 :
                   sqrt(max(0.0, num.m2) / (num.col.n - 1))
spread(sym::Sym) = -sum(
  let prob = val/sym.col.n; prob*log2(prob) end
  for val in values(sym.has) if val > 0; init=0.0)

z(num, val)         = clamp((val - num.mu) / (spread(num) + 1e-30), -3.0, 3.0)
norm(sym::Sym, val) = val
norm(num::Num, val) = unk(val) ? val : 1.0 / (1.0 + exp(-1.7 * z(num,val)))

#-- distance --------------------------------------------------------
function minkowski(vals)
  (sum(val^the[:p] for val in vals) / (1E-32 + length(vals))) ^ (1/the[:p])  end

function disty(data, row)
  minkowski([norm(y, cell(y,row)) - y.col.heaven for y in data.cols.ys])  end

function aha(col, u, v)
  unk(u) && unk(v) && return 1.0
  col isa Sym && return u == v ? 0.0 : 1.0
  nu, nv = norm(col,u), norm(col,v)
  unk(nu) && (nu = nv > 0.5 ? 0.0 : 1.0)
  unk(nv) && (nv = nu > 0.5 ? 0.0 : 1.0)
  abs(nu - nv)  end

distx(data, row1, row2) =
  minkowski([aha(x, cell(x,row1), cell(x,row2)) for x in data.cols.xs])

order(data, row, rows)    = sort(rows, by=r -> distx(data,row,r))
nearest(data, row, rows)  = first(order(data,row,rows))
furthest(data, row, rows) = last(order(data,row,rows))

function nearby(sym::Sym, val)
  n = rand() * sum(values(sym.has))
  for (key,v) in sym.has 
    if (n -= v) <= 0 return key end end end

nearby(num::Num, val) =
  (unk(val) ? num.mu : val) + 2*spread(num)*(sum(rand() for _ in 1:3) - 1.5)

#-- bayes -----------------------------------------------------------
prior(data, n_all, n_h) = (data.n + the[:m]) / (n_all + the[:m] * n_h)

function like(data::Data, row, n_all, n_h)
  pr = prior(data, n_all, n_h)
  log(pr) + sum(
    let l = like(x, cell(x,row), pr); l > 0 ? log(l) : 0.0 end
    for x in data.cols.xs if !unk(cell(x,row)))  end

function like(num::Num, val, _)
  var = spread(num)^2 + 1e-30
  exp(-(val - num.mu)^2 / (2var)) / sqrt(2π * var)  end

function like(sym::Sym, val, prior) 
  max(1e-32, 
      (get(sym.has,val,0.0) + the[:k]*prior) / (sym.col.n + the[:k])) end

#-- demos -----------------------------------------------------------

eg=Dict()
eg["-h"]    = _     -> println(HELP)
eg["--the"] = _     -> println(the)
eg["--all"] = (val) -> [run(fun,val) for (k,fun) in eg if k !== "--all"]

eg["--csv"] = function(file)
  rows = []
  csv(file, row -> push!(rows, row))
  align(eachn(rows))  end

eg["--data"] = function(file)
  data = data!(file)
  align([data.cols.strs; [mids(data)]; eachn(data.rows)])  end

eg["--disty"] = function(file)
  data = data!(file)
  align([data.cols.strs;
         eachn(sort(data.rows, by=row -> disty(data,row)))])  end

eg["--addsub"] = function(file)
  data, one, two = data!(file), nothing, nothing
  rows = copy(data.rows)
  for row in reverse(rows)
    sub!(data,row); data.n==50 && (one = mids(data)) end
  for row in rows
    add!(data,row); data.n==50 && (two = mids(data)) end
  @assert all(!(a isa Float64) || abs(a-b) < 1e-5 for (a,b) in zip(one,two))
  println("ok")  end

eg["--bayes"] = function(file)
  data = data!(file)
  for row in eachn(data.rows)
    println(say(round(likes(data,row,data.n,1) * 100) / 100))  end  end

#-- main ------------------------------------------------------------
function run(fun, val) Random.seed!(the[:seed]); fun(val) end

function cli(the, args)
  i, n = 1, length(args)
  while i <= n
    key, s = args[i], Symbol(lstrip(args[i], '-'))
    if haskey(eg, key)
      run(eg[key], i < n ? make(args[i + 1]) : nothing); i += 2
    elseif haskey(the, s) && i < n 
      the[s] = make(args[i + 1]); i += 2
    else i += 1 end end end

endswith(PROGRAM_FILE, "ezr.jl") &&  cli(the,ARGS) 

# if abspath(PROGRAM_FILE)==@__FILE__
#   Random.seed!(the["seed"]); args=copy(ARGS)
#   EG=Dict(replace(string(f),"eg_"=>"")=>f
#        for f in [eg_data,eg_ranks])
#   while !isempty(args)
#     m=match(r"^--?(.+)",popfirst!(args))
#     m===nothing && continue
#     k=m[1]; Random.seed!(the["seed"])
#     if haskey(EG,k)
#       fn=EG[k]
#       na=length(first(methods(fn)).sig.parameters)-1
#       fn([popfirst!(args) for _ in 1:na]...)
#     elseif haskey(the,k)
#       the[k]=thing(popfirst!(args))
#     end end end
