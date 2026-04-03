# - My crystal style rules.
# - Never `end` on its own line; join to prior line or use semicolons
# - Classes hold ONLY: property declarations, initialize, private methods
# - All polymorphic/shared methods are top-level functions using
#   multi-dispatch, grouped by name (all `add` together, all `mid` together)
# - One-liner methods: `def f(x : T) : R;  expr end`
# - Compress property: `property a = 0, b = 0.0, c : Bool`
# - Compress init: `def initialize(@x = 0); super; @y = calc end end`
# - Explicit return types on all functions
# - Minimize LOC; semicolons over newlines when readable
# - `i` for self/first-arg in dispatched functions
# - Factory/construction logic in top-level functions, not in classes
# - Align related assignments with spaces for readability

include Math
alias Value = Float64 | String
alias Row = Array(Value)

# ---------------------------------------------------------------------
# ## Classes

# Global settings.
class Obj
  property decs = 2, seed = 1, p = 2, budget = 50, k = 1
  property m = 2, few = 128, leaf = 3, treeWidth = 30, check = 5, start = 4
  property file = "#{Path.home}/gits/moot/optimize/misc/auto93.csv" end

THE = Obj.new

# Summarize column of values.
abstract class Col
  property at : Int32, txt : String, n = 0.0
  def initialize(@at = 0, @txt = ""); end end 

# Summarize stream of numbers.
class Num < Col
  property mu = 0.0, m2 = 0.0, sd = 0.0, heaven : Float64
  def initialize(@at = 0, @txt = "")
    super; @heaven = @txt.ends_with?("-") ? 0 : 1 end end

# Summarize stream of symbls.
class Sym < Col
  property has = Hash(String, Int32).new(0) end

# Factory. Strings to Cols. Upper/lower case names ==> Num/Sym.
class Cols
  property names : Array(String), all : Array(Col)
  property xs : Array(Col), ys : Array(Col), klass : Col?
  def initialize(@names)
    @all   = @names.map_with_index { |txt, j|
               txt[0].uppercase? ? Num.new(j, txt) : Sym.new(j, txt) }
    @klass = @all.find    { |c| c.txt.ends_with?("!") }
    @xs    = @all.reject  { |c| "+-!X".includes?(c.txt[-1]) }
    @ys    = @all.select  { |c| "+-!".includes?(c.txt[-1]) } end end

class Data
  property rows : Array(Row), cols : Cols
  property _mids : Array(Value)?, n = 0.0
  def initialize(header : Array(String))
    @rows = [] of Row
    @cols = Cols.new(header) end end

# ---------------------------------------------------------------------
# ## Data (factory overloads) 

# Array(Row) ==>  Data.
def data(src : Array(Row)) : Data
  d = Data.new(src.first.map(&.as(String)))
  src[1..].each { |r| add(d, r) }; d end

# Make a new Data, modelied on an old one. Maybe add rows.
def data(d : Data, rows = [] of Row) : Data
  d2 = Data.new(d.cols.names)
  rows.each { |r| add(d2, r) }; d2 end

  # Load in a Data from a csv files.
def data(file : String) : Data
  rows = [] of Row
  csv(file) { |r| rows << r }
  data(rows) end

#  --------------------------------------------------------------
# ## Sub, add, add1 

# `sub` means adding -1.
def sub(i : Data | Col, v); add(i, v, -1) end

# Add each of `src` into something (usually a Num).
def add(src : Enumerable, i :  Col | Data = Num.new) : Col | Data
  src.each { |v| add(i, v) }; i end

# Update a Data's rows and column summary.
def add(d : Data, r : Row, w = 1) : Row
  c.all.each { |col| add(col, r[col.at], w) }
  d.rows << r
  d._mids = nil # now outdated
  r end

# Update a column. Ignore "?". Return the added item `v`.
def add(i : Col, v : Value, w = 1) : Value
  if v != "?"
    i.n += w
    _add1(i, v, w) end
  v end

def _add1(i : Sym, v : Value, w : Int32) 
  i.has[v.as(String)] += w end

def _add1(i : Num, v : Value, w : Int32)
  f = v.as(Float64)
  if w<0 && i.n<=1 i.n=i.mu=i.m2=i.sd=0.0
  else 
    d    = f - i.mu
    i.mu += w * d / i.n
    i.m2 += w * d * (f - i.mu)
    i.sd  = i.n < 2 ? 0.0 : (i.m2 / (i.n - 1)).abs.sqrt end end

# ---------------------------------------------------------------------
# Query 
def like(c : Num, v : Value, prior : Float64) : Float64
  sd = c.sd + 1e-32
  (1 / sqrt(2*PI*sd*sd)) * exp(-((v.as(Float64) - c.mu)**2) / (2*sd*sd)) end

def like(c : Sym, v : Value, prior : Float64) : Float64
  (c.has.fetch(v.as(String), 0) + THE.k * prior) / (c.n + THE.k) end

def like(d : Data, r : Row, n_rows : Int32, n_klasses : Int32) : Float64
  prior = (d.rows.size + THE.m) / (n_rows + THE.m * n_klasses).to_f
  out   = log(prior)
  d.cols.xs.each { |c|
    v = r[c.at]
    if v != "?"
      l = like(c, v, prior)
      out += log(l) if l > 0 end }
  out end
 
# ---------------------------------------------------------------------
# ## Minkowski, distx, disty, wins 
def wins(d : Data) : Proc(Row, Int32)
  d.rows.sort_by! { |r| disty(d,r) }
  lo = disty(d,d.rows[0])
  md = dist(d,d.rows[d.rows.size // 2])
  ->(r : Row) { (100 * (1 - (disty(d,r) - lo) / (md - lo + 0.0001))).to_i } end

def disty(d : Data, r : Row) : Float64
  _minkowski(d.cols.ys.map { |c| (norm(c,r[c.at])-c.heaven).abs }) end

def distx(d : Data, r1 : Row, r2 : Row) : Float64
  _minkowski(d.cols.xs.map { |c| _aha(c, r1[c.at], r2[c.at]) }) end

def _minkowski(items : Enumerable(Float64), p = 2) : Float64
  tot, n = 0.0, 1e-32
  items.each { |x| tot += x ** p; n += 1.0 }
  (tot / n) ** (1.0 / p) end

def _aha(i : Num, u : Value, v : Value) : Float64
  return 1.0 if u == "?" && v == "?"
  nu = u == "?" ? u : norm(i, u)
  nv = v == "?" ? v : norm(i, v)
  a = nu == "?" ? (nv.as(Float64) > 0.5 ? 0.0 : 1.0) : nu.as(Float64)
  b = nv == "?" ? (a > 0.5 ? 0.0 : 1.0) : nv.as(Float64)
  (a - b).abs end

def _aha(i : Sym, u : Value, v : Value) : Float64
  return 1.0 if u == "?" && v == "?"
  u != v ? 1.0 : 0.0 end

# ---------------------------------------------------------------------
# ## Acquire 
def acquireWithBayes(d : Data, best : Data, rest : Data, r : Row) : Float64
  n = best.rows.size + rest.rows.size
  like(rest, r, n, 2) - like(best, r, n, 2) end

def acquireWithCentroid(d : Data, best : Data, rest : Data, r : Row) : Float64
  distx(d, r, mid(best)) - distx(d, r, mid(rest)) end

def acquire(d : Data, score = ->acquireWithBayes(Data, Data, Data, Row)) : Data
  rows  = d.rows.shuffle(Random.new(THE.seed))
  lab   = data(d, rows[0...THE.start])
  unlab = rows[THE.start..][0...THE.few]
  lab.rows.sort_by! { |r| disty(lab, r) }
  n    = sqrt(lab.rows.size).to_i
  best, rest = data(d, lab.rows[0...n]), data(d, lab.rows[n..])
  cursor = 0
  THE.budget.times do
    j = (0...unlab.size).find { |j| score.call(lab, best, rest, unlab[(cursor+j) % unlab.size]) < 0 }
    break unless j
    idx = (cursor + j) % unlab.size
    r = unlab.delete_at(idx)
    add(lab, add(best, r))
    if best.rows.size > sqrt(lab.rows.size)
      best.rows.sort_by! { |rr| disty(lab, rr) }
      add(rest, sub(best, best.rows.last)) end
    cursor = idx end
  lab.rows.sort_by! { |r| disty(lab, r) }
  lab end
 
# ---------------------------------------------------------------------
# ## Tree clas 
class Tree
  property d : Data, ynum : Num
  property col : Col?, cut : Value
  property left : Tree?, right : Tree?
  def initialize(@d, @ynum)
    @col, @cut, @left, @right = nil, 0.0, nil, nil end end

def treeCuts(c : Num, rs : Array(Row)) : Array(Value)
  vs = rs.compact_map { |r| v = r[c.at]; v != "?" ? v : nil }
  vs.empty? ? [] of Value : [vs.sort.as(Array(Value))[vs.size // 2]] end

def treeCuts(c : Sym, rs : Array(Row)) : Array(Value)
  rs.compact_map { |r| v = r[c.at]; v != "?" ? v : nil }.uniq end

def treeSplit(d : Data, c : Col, cut : Value, rs : Array(Row))
  l_rs, r_rs, l_num, r_num = [] of Row, [] of Row, Num.new, Num.new
  rs.each { |r|
    v = r[c.at]
    go = v == "?" || (c.is_a?(Num) ? v.as(Float64) <= cut.as(Float64) : v == cut)
    (go ? l_rs : r_rs) << r
    add(go ? l_num : r_num, disty(d, r)) }
  {l_num.n * spread(l_num) + r_num.n * spread(r_num), c, cut, l_rs, r_rs} end

def treeGrow(d : Data, rs : Array(Row)) : Tree
  t = Tree.new(data(d, rs), add(rs.map { |r| disty(d, r) }).as(Num))
  if rs.size >= 2 * THE.leaf
    splits = d.cols.not_nil!.xs.flat_map { |c|
      treeCuts(c, rs).map { |cut| treeSplit(d, c, cut, rs) } }
    valid = splits.select { |s| min(s[3].size, s[4].size) >= THE.leaf }
    unless valid.empty?
      best = valid.min_by { |s| s[0] }
      t.col, t.cut = best[1], best[2]
      t.left  = treeGrow(d, best[3])
      t.right = treeGrow(d, best[4]) end end
  t end

def treeLeaf(t : Tree, r : Row) : Tree
  return t unless t.left
  v = r[t.col.not_nil!.at]
  go = t.col.is_a?(Num) ? (v != "?" && v.as(Float64) <= t.cut.as(Float64)) :
                           (v != "?" && v == t.cut)
  treeLeaf(go ? t.left.not_nil! : t.right.not_nil!, r) end

def treeNodes(t : Tree, lvl = 0, col : Col? = nil, op = "", cut : Value? = nil, &)
  yield t, lvl, col, op, cut
  if c = t.col
    ops = c.is_a?(Num) ? {"<=", ">"} : {"==", "!="}
    kids = [{t.left.not_nil!, ops[0]}, {t.right.not_nil!, ops[1]}]
      .sort_by { |k| mid(k[0].ynum).as(Float64) }
    kids.each { |k, s| treeNodes(k, lvl + 1, c, s, t.cut) { |*a| yield *a } } end end

def treeShow(t : Tree)
  treeNodes(t) { |t1, lvl, col, op, cut|
    p = col ? "#{col.txt} #{op} #{cut}" : ""
    p = "|   " * (lvl - 1) + p if lvl > 0
    g = t1.d.cols.not_nil!.ys.map { |c| "#{c.txt}=#{mid(c)}" }.join(", ")
    puts "#{p.ljust(THE.treeWidth)},#{mid(t1.ynum).to_s.rjust(5)} ,(#{t1.ynum.n.to_s.rjust(3)}), {#{g}}" } end

# ---------------------------------------------------------------------
# Lib 
def thing(s : String) : Value
  s = s.strip
  s.to_f64? ? s.to_f64 : s end

def csv(file : String, &)
  File.each_line(file) { |line|
    cells = line.partition("#")[0].split(",")
    if cells.any? { |s| s.strip != "" }
      yield cells.map { |s| thing(s) } end } end

# ---------------------------------------------------------------------
# ## Ready 
def ready(file : String)
  d = data(file)
  d.rows.shuffle!(Random.new(THE.seed))
  n = d.rows.size // 2
  {d, data(d, d.rows[0...n][0...THE.budget]), d.rows[n..]} end

# ---------------------------------------------------------------------
# ## Main 
d, train, test = ready(THE.file)
w = wins(d)
lab = acquire(train)
lab.rows.sort_by! { |r| disty(lab, r) }
puts w.call(lab.rows[0])

