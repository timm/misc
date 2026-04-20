#!/usr/bin/env julia --startup-file=no  --compile=min -O0 
# ezr.jl -- data-lite active learning.
# (c) 2026 Tim Menzies, timm@ieee.org, MIT.
#
# USAGE
#   julia ezr.jl --cmd [arg]  [-flag value]...
#
# CMDS  (--all runs every --xxx command)
#   --the     print config
#   --rows    read CSV, print a few rows
#   --data    build Data, show y-columns
#   --stats   mid and spread per column
#   --norm    raw vs normalized y-values of row 1
#   --ydata   sort rows by goal-distance
#   --xdata   sort rows by feature-distance from row 1
#   --tree    active-label, grow tree, print it
#   --active  20 train/test splits; prints: train hold labels
#
# CSV INPUT. Header row names columns; suffix sets role.
#   [A-Z]*   numeric           [a-z]*   symbolic
#   [A-Z]*-  minimize (y)      [A-Z]*+  maximize (y)
#   *!       class (y)         *X       skip
#   ?        missing value

using Printf, Random

const THE = [
  (:p,     2,             "-p",      "Minkowski exponent"),
  (:f,     "auto93.csv",  "-f",      "CSV file"),
  (:s,     1,             "-s",      "random seed"),
  (:b,     50,            "-b",      "label budget"),
  (:u,     128,           "-u",      "unlabelled pool cap"),
  (:check, 5,             "--check", "holdout pay"),
  (:leaf,  3,             "--leaf",  "tree leaf size"),
]
const the      = Dict{Symbol,Any}(t[1] => t[2] for t in THE)
const flag2key = Dict{String,Symbol}(t[3] => t[1] for t in THE)

@kwdef mutable struct Sym
  at::Int=0; txt::String=" "; n::Int=0; has=Dict() end

@kwdef mutable struct Num
  at::Int=0; txt::String=" "; n::Int=0
  mu::Float64=0.0; m2::Float64=0.0; heaven::Int=1 end

mutable struct Cols
  names::Vector{String}; all::Vector; x::Vector; y::Vector end

@kwdef mutable struct Data
  rows::Vector=Any[]; cols=nothing; mid=nothing end

@kwdef mutable struct Tree
  d=nothing; ynum=nothing; col=nothing; cut=nothing
  left=nothing; right=nothing; hdr="" end

#=================================================
# PART 2 -- Utils, Constructors, Add/Sub
#=================================================
unk(v)        = v == "?"
cell(c, row)  = row[c.at]
eachn(rs,n=30)= rs[1:n:end]

function thing(s)
  s = strip(string(s))
  for T in (Int, Float64, Bool)
    n = tryparse(T, s); n !== nothing && return n end
  s == "?" ? "?" : s end

function csv_rows(file)
  rs = Any[]
  open(file) do f
    for line in eachline(f)
      line = strip(split(line, "#")[1])
      isempty(line) && continue
      push!(rs, [thing(x) for x in split(line, ",")]) end end
  rs end

function shuffled(rs)
  v = copy(rs)
  for i in length(v):-1:2
    j = rand(1:i); v[i], v[j] = v[j], v[i] end
  v end

function make_col(i, txt)
  isuppercase(first(txt)) ?
    Num(at=i, txt=txt, heaven=endswith(txt,"-") ? 0 : 1) :
    Sym(at=i, txt=txt) end

function make_cols(names)
  all = [make_col(i, t) for (i, t) in enumerate(names)]
  x, y = Any[], Any[]
  for c in all
    endswith(c.txt, "X") && continue
    occursin(r"[!+\-]$", c.txt) ? push!(y, c) : push!(x, c) end
  Cols(names, all, x, y) end

function make_data(rows=nothing)
  d = Data()
  rows === nothing && return d
  for r in rows; add!(d, r); end
  d end

sub!(it, v) = add!(it, v, -1)

function add!(s::Sym, v, w=1)
  unk(v) && return v
  s.n += w
  s.has[v] = w + get(s.has, v, 0)
  v end

function add!(n::Num, v, w=1)
  unk(v) && return v
  if w < 0 && n.n <= 2
    n.n = 0; n.mu = 0.0; n.m2 = 0.0
  else
    n.n += w
    d = v - n.mu
    n.mu += w * d / n.n
    n.m2 += w * d * (v - n.mu) end
  v end

function add!(cs::Cols, row, w=1)
  for (c, v) in zip(cs.all, row); add!(c, v, w); end
  row end

function add!(d::Data, row, w=1)
  d.mid = nothing
  if d.cols === nothing
    d.cols = make_cols(row)
  else
    add!(d.cols, row, w)
    w > 0 ? push!(d.rows, row) :
            deleteat!(d.rows, findfirst(==(row), d.rows)) end
  row end

adds!(vs, s=Num()) = (for v in vs; add!(s, v); end; s)

#=================================================
# PART 3 -- Stats & Distance
#=================================================
mid(n::Num) = n.mu
mid(s::Sym) = argmax(s.has)
function mid(d::Data)
  d.mid === nothing &&
    (d.mid = [mid(c) for c in d.cols.all])
  d.mid end

spread(n::Num) = n.n < 2 ? 0.0 :
  sqrt(max(0.0, n.m2) / (n.n - 1))
spread(s::Sym) = -sum(let p = v / s.n; p * log2(p) end
  for v in values(s.has) if v > 0; init=0.0)

norm(::Sym, v) = v
function norm(n::Num, v)
  unk(v) && return v
  z = (v - n.mu) / (spread(n) + 1e-32)
  1 / (1 + exp(-1.7 * clamp(z, -3, 3))) end

function minkowski(vs)
  p = the[:p]; tot = 0.0; k = 0
  for v in vs; tot += v^p; k += 1; end
  (tot / max(1, k))^(1 / p) end

disty(d, row) = minkowski(
  abs(norm(c, cell(c, row)) - c.heaven) for c in d.cols.y)

function aha(::Sym, a, b)
  unk(a) && unk(b) && return 1.0
  a == b ? 0.0 : 1.0 end

function aha(c::Num, a, b)
  unk(a) && unk(b) && return 1.0
  a, b = norm(c, a), norm(c, b)
  unk(a) && (a = b > 0.5 ? 0.0 : 1.0)
  unk(b) && (b = a > 0.5 ? 0.0 : 1.0)
  abs(a - b) end

distx(d, r1, r2) = minkowski(
  aha(c, cell(c, r1), cell(c, r2)) for c in d.cols.x)

#=================================================
# PART 4 -- Scoring & Trees
#=================================================
function wins(god)
  ys = sort([disty(god, r) for r in god.rows])
  lo, md = ys[1], ys[length(ys) ÷ 2 + 1]
  r -> floor(Int, 100 *
    (1 - (disty(god, r) - lo) / (md - lo + 1e-32))) end

function new_tree(d, rs, hdr="")
  sub = Data()
  add!(sub, d.cols.names)
  for r in rs; add!(sub, r); end
  ys = Num()
  for r in rs; add!(ys, disty(d, r)); end
  Tree(d=sub, ynum=ys, hdr=hdr) end

go_left(::Sym, v, cut) = unk(v) || v == cut
go_left(::Num, v, cut) = unk(v) || v <= cut

cand_cuts(::Sym, vs) = unique(vs)
cand_cuts(::Num, vs) = [sort(vs)[length(vs) ÷ 2 + 1]]

function tree_cuts(c, rs)
  vs = Any[]
  for r in rs
    v = cell(c, r); unk(v) || push!(vs, v) end
  isempty(vs) ? [] : cand_cuts(c, vs) end

function tree_split(d, c, cut, rs)
  l, r = Any[], Any[]; ln, rn = Num(), Num()
  for row in rs
    if go_left(c, cell(c, row), cut)
      push!(l, row); add!(ln, disty(d, row))
    else
      push!(r, row); add!(rn, disty(d, row)) end end
  (ln.n * spread(ln) + rn.n * spread(rn), c, cut, l, r) end

function kid_hdr(c, cut, left)
  sym = c isa Sym
  op = left ? (sym ? "==" : "<=") : (sym ? "!=" : ">")
  "$(c.txt) $op $cut" end

function tree_grow(d, rs, leaf=the[:leaf], hdr="")
  t = new_tree(d, rs, hdr)
  length(rs) >= 2 * leaf || return t
  opts = Any[]
  for c in d.cols.x, cut in tree_cuts(c, rs)
    s = tree_split(d, c, cut, rs)
    min(length(s[4]), length(s[5])) >= leaf && push!(opts, s) end
  isempty(opts) && return t
  best = argmin(o -> o[1], opts)
  _, c, cut, l, r = best
  t.col = c; t.cut = cut
  t.left  = tree_grow(d, l, leaf, kid_hdr(c, cut, true))
  t.right = tree_grow(d, r, leaf, kid_hdr(c, cut, false))
  t end

function tree_leaf(t, row)
  t.left === nothing && return t
  c = t.col
  go_left(c, cell(c, row), t.cut) ?
    tree_leaf(t.left, row) : tree_leaf(t.right, row) end

function tree_show(t, lvl=0)
  bars = repeat("|.. ", max(0, lvl - 1))
  @printf("%.2f (%3d)   %s%s\n",
    mid(t.ynum), t.ynum.n, bars, t.hdr)
  if t.left !== nothing
    tree_show(t.left, lvl + 1); tree_show(t.right, lvl + 1) end end

#=================================================
# PART 5 -- Active Learning
#=================================================
closer(lab, best, rest, r) =
  distx(lab, r, mid(best)) < distx(lab, r, mid(rest))

function rebalance!(best, rest, lab)
  length(best.rows) > sqrt(length(lab.rows)) || return
  bad = argmax(r -> disty(lab, r), best.rows)
  sub!(best, bad); add!(rest, bad) end

function warm_start!(rs, lab, best, rest, ys)
  for _ in 1:min(4, length(rs))
    r = popfirst!(rs)
    add!(lab, r); add!(ys, disty(lab, r)) end
  sorted = sort(lab.rows, by = r -> disty(lab, r))
  n = max(1, Int(floor(sqrt(length(sorted)))))
  for r in sorted[1:n];       add!(best, r); end
  for r in sorted[n+1:end];   add!(rest, r); end
  rs end

function active(rs)
  hd   = first(rs)
  lab  = make_data([hd])
  best = make_data([hd])
  rest = make_data([hd])
  ys   = Num()
  pool = warm_start!(
    shuffled(rs[2:end]), lab, best, rest, ys)
  for r in pool[1:min(the[:u], length(pool))]
    ys.n >= the[:b] && break
    if closer(lab, best, rest, r)
      add!(ys, disty(lab, r))
      add!(lab, r); add!(best, r)
      rebalance!(best, rest, lab) end end
  best, lab, ys.n end

function validate(rs, god, w)
  check = the[:check]
  body  = shuffled(rs[2:end])
  n     = length(body) ÷ 2
  train = vcat([first(rs)], body[1:n])
  test  = body[n+1:end]
  _, lab, labels = active(train)
  y_god = r -> disty(god, r)
  train_best = argmin(y_god, lab.rows)
  tr = tree_grow(lab, lab.rows)
  sorted = sort(test,
    by = r -> mid(tree_leaf(tr, r).ynum))
  top  = sorted[1:min(check, length(sorted))]
  pick = argmin(y_god, top)
  w(train_best), w(pick), labels + check end

#=================================================
# PART 6 -- Examples & CLI
#=================================================
const eg = Dict{String,Function}()

eg["--the"]    = _ -> println(the)

eg["--rows"]   = function(file)
  rs = csv_rows(something(file, the[:f]))
  println(length(rs), " rows")
  for r in rs[max(1, end-5):end]; println(r); end end

eg["--data"]   = function(file)
  d = make_data(csv_rows(something(file, the[:f])))
  for c in d.cols.y; println(c); end end

eg["--stats"]  = function(file)
  d = make_data(csv_rows(something(file, the[:f])))
  for c in d.cols.all
    @printf("%-12s mid=%s spread=%s\n",
      c.txt, mid(c), spread(c)) end end

eg["--norm"]   = function(file)
  d = make_data(csv_rows(something(file, the[:f])))
  r = first(d.rows)
  for c in d.cols.y
    v = cell(c, r)
    @printf("%-8s raw=%s norm=%s\n", c.txt, v, norm(c, v)) end end

eg["--ydata"]  = function(file)
  d = make_data(csv_rows(something(file, the[:f])))
  for r in eachn(sort(d.rows, by = r -> disty(d, r)))
    println(r) end end

eg["--xdata"]  = function(file)
  d = make_data(csv_rows(something(file, the[:f])))
  r0 = first(d.rows)
  for r in eachn(sort(d.rows, by = r -> distx(d, r0, r)))
    println(r) end end

eg["--tree"]   = function(file)
  rs = csv_rows(something(file, the[:f]))
  _, lab, labels = active(rs)
  println("# labels used: ", labels)
  tree_show(tree_grow(lab, lab.rows)) end

eg["--active"] = function(file)
  rs  = csv_rows(something(file, the[:f]))
  god = make_data(rs)
  w   = wins(god)
  for _ in 1:20
    t, h, l = validate(rs, god, w)
    @printf("%3d %3d %3d\n", t, h, l) end end

eg["--all"]    = function(arg)
  for (k, fn) in eg
    k in ("--all",) && continue
    Random.seed!(the[:s]); fn(arg) end end

function cli()
  args = copy(ARGS)
  while !isempty(args)
    flag = popfirst!(args)
    if haskey(eg, flag)
      arg = !isempty(args) && !startswith(args[1], "-") ?
            popfirst!(args) : nothing
      Random.seed!(the[:s]); eg[flag](arg)
    elseif haskey(flag2key, flag) && !isempty(args)
      the[flag2key[flag]] = thing(popfirst!(args)) end end end

cli()
