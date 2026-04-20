#!/usr/bin/env crystal
# ezr.cr : data-lite active learning.
# (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
#
# USAGE
#   crystal build --release ezr.cr -o ezr_cr
#   ./ezr_cr --cmd [arg]   [-flag value]...
#
# CMDS
#   --the     print config
#   --csv     dump every 30th row of the CSV
#   --data    build Data, show y-column mids
#   --tree    random-label, grow tree, pretty-print
#   --active  20 train/test splits; prints: train hold labels
#
# OPTIONS
#   -B Budget=50     label budget
#   -C Check=5       final holdout check size
#   -f few=128       max unlabelled pool
#   -l leaf=3        min rows per tree leaf
#   -p p=2           Minkowski distance coefficient
#   -s seed=1        random seed
#   -S Show=30       tree-display column width
#
# CSV INPUT
#   First row names columns; suffix sets the role.
#     [A-Z]*   numeric           [a-z]*   symbolic
#     [A-Z]*-  minimize (y)      [A-Z]*+  maximize (y)
#     *!       class (y)         *X       skip
#     ?        missing value

alias Val = Float64 | String
alias Row = Array(Val)

HELP = <<-EOS
ezr.cr : explainable multi-objective optimization

USAGE
  ./ezr_cr --cmd [arg]   [-flag value]...

CMDS
  --the     print config
  --csv     dump every 30th row
  --data    build Data, show y-column mids
  --tree    random-label, grow tree, pretty-print
  --active  20 train/test splits; prints: train hold labels

OPTIONS
  -B Budget=50     -C Check=5       -f few=128
  -l leaf=3        -p p=2           -s seed=1        -S Show=30
EOS

module The
  class_property budget = 50
  class_property check  = 5
  class_property few    = 128
  class_property leaf   = 3
  class_property p      = 2
  class_property seed   = 1_i64
  class_property show   = 30 end

module RNG
  @@rng = Random.new(1_i64)
  @@rowid = 0
  def self.get; @@rng; end
  def self.reseed(s : Int64); @@rng = Random.new(s.to_u64); end
  def self.next_id; @@rowid += 1; @@rowid; end end

# ## structs ---------------------------------------------------
abstract class Col
  property at : Int32 = 0
  property txt : String = ""
  property n : Int32 = 0
  property heaven : Int32 = 1
  abstract def add(v : Val)
  abstract def sub1(v : Val)
  abstract def mid : Val
  abstract def spread : Float64
  abstract def norm(v : Val) : Val
  abstract def aha(a : Val, b : Val) : Float64
  abstract def splits(rs : Array(Row), fn : Row -> Float64) : Array(Split) end

class Sym < Col
  property has = Hash(Val, Int32).new(0)
  def initialize(@txt = "", @at = 0); end
  def add(v : Val); @has[v] += 1; @n += 1; end
  def sub1(v : Val); @has[v] -= 1; @n -= 1; end
  def mid : Val
    best = -1
    out : Val = ""
    @has.each { |k, v| if v > best; best, out = v, k; end }
    out end
  def spread : Float64
    return 0.0 if @n == 0
    -@has.values.sum { |v| r = v / @n.to_f; r > 0 ? r * Math.log2(r) : 0.0 } end
  def norm(v : Val) : Val; v; end
  def aha(a : Val, b : Val) : Float64
    return 1.0 if a == "?" && b == "?"
    a == b ? 0.0 : 1.0 end
  def splits(rs : Array(Row), fn : Row -> Float64) : Array(Split)
    seen = Set(Val).new
    out  = [] of Split
    rs.each do |r|
      v = r[@at]
      next if v == "?" || seen.includes?(v)
      seen << v
      sp = Ezr.split(self, rs, fn, v) { |x| x == v }
      out << sp if sp end
    out end end

class Num < Col
  property mu = 0.0
  property m2 = 0.0
  def initialize(@txt = "", @at = 0)
    @heaven = @txt.ends_with?("-") ? 0 : 1 end
  def add(v : Val)
    return unless v.is_a?(Float64)
    @n += 1
    d = v - @mu
    @mu += d / @n
    @m2 += d * (v - @mu) end
  def sub1(v : Val)
    return unless v.is_a?(Float64)
    if @n <= 1
      @n = 0; @mu = 0.0; @m2 = 0.0
    else
      @n -= 1
      d = v - @mu
      @mu -= d / @n
      @m2 -= d * (v - @mu) end end
  def mid : Val; @mu; end
  def spread : Float64
    @n > 1 ? Math.sqrt({0.0, @m2}.max / (@n - 1)) : 0.0 end
  def norm(v : Val) : Val
    return v if v == "?"
    z = (v.as(Float64) - @mu) / (spread + 1e-32)
    z = z.clamp(-3.0, 3.0)
    1.0 / (1.0 + Math.exp(-1.7 * z)) end
  def aha(a : Val, b : Val) : Float64
    return 1.0 if a == "?" && b == "?"
    a2 = a == "?" ? a : norm(a)
    b2 = b == "?" ? b : norm(b)
    a2 = (b2.as(Float64) > 0.5 ? 0.0 : 1.0) if a2 == "?"
    b2 = (a2.as(Float64) > 0.5 ? 0.0 : 1.0) if b2 == "?"
    (a2.as(Float64) - b2.as(Float64)).abs end
  def splits(rs : Array(Row), fn : Row -> Float64) : Array(Split)
    vs = [] of Float64
    rs.each { |r| v = r[@at]; vs << v.as(Float64) if v != "?" }
    return [] of Split if vs.size < 2
    vs.sort!
    mu = vs[vs.size // 2]
    sp = Ezr.split(self, rs, fn, mu) { |x| x != "?" && x.as(Float64) <= mu }
    sp ? [sp] : [] of Split end end

class Cols
  getter names : Array(String)
  getter all   = [] of Col
  getter x     = [] of Col
  getter y     = [] of Col
  property klass : Col?
  def initialize(@names)
    @names.each_with_index do |s, at|
      col = (s[0]? && s[0].ascii_uppercase?) ? Num.new(s, at).as(Col) : Sym.new(s, at).as(Col)
      @all << col
      @klass = col if s.ends_with?("!")
      next if s.ends_with?("X")
      (s.ends_with?("+") || s.ends_with?("-") || s.ends_with?("!") ? @y : @x) << col end end end

class Data
  property rows = [] of Row
  property cols : Cols?
  property mid_cache : Array(Val)?
  def initialize(src : String | Array(Row) | Nil = nil)
    if src.is_a?(String)
      Ezr.csv_each(src) { |r| add(r) }
    elsif src.is_a?(Array(Row))
      src.each { |r| add(r) } end end
  def add(row : Row)
    if (c = @cols).nil?
      @cols = Cols.new(row.map &.as(String))
    else
      @mid_cache = nil
      c.all.each { |col| col.add(row[col.at]) unless row[col.at] == "?" }
      @rows << row end end
  def sub(row : Row)
    @mid_cache = nil
    if c = @cols
      c.all.each { |col| col.sub1(row[col.at]) unless row[col.at] == "?" }
      idx = @rows.index { |r| r.same?(row) }
      @rows.delete_at(idx) if idx end end
  def mid : Array(Val)
    @mid_cache ||= @cols.not_nil!.all.map &.mid
    @mid_cache.not_nil! end
  def clone_with(rs : Array(Row) = [] of Row) : Data
    d = Data.new
    d.add([@cols.not_nil!.names.map(&.as(Val))].first) # header
    rs.each { |r| d.add(r) }
    d end
  def disty(row : Row) : Float64
    c = @cols.not_nil!
    tot = c.y.sum { |col| ((col.norm(row[col.at])).as(Float64) - col.heaven.to_f).abs ** The.p }
    (tot / c.y.size) ** (1.0 / The.p) end
  def distx(r1 : Row, r2 : Row) : Float64
    c = @cols.not_nil!
    tot = c.x.sum { |col| col.aha(r1[col.at], r2[col.at]) ** The.p }
    (tot / c.x.size) ** (1.0 / The.p) end end

class Split
  property col : Col
  property cut : Val
  property left  = [] of Row
  property right = [] of Row
  property lhs   : Num
  property rhs   : Num
  def initialize(@col, @cut, @left, @right, @lhs, @rhs); end end

class Tree
  property score : Row -> Float64
  property y     = Num.new("y")
  property mids  = {} of String => Val
  property col   : Col?
  property cut   : Val = 0.0.as(Val)
  property at    = 0
  property left  : Tree?
  property right : Tree?
  def initialize(@score); end

  def build(d : Data, rs : Array(Row)) : Tree
    mid = d.clone_with(rs).mid
    rs.each { |r| @y.add(@score.call(r)) }
    d.cols.not_nil!.y.each { |c| @mids[c.txt] = mid[c.at] }
    return self if rs.size < 2 * The.leaf
    best : Split? = nil
    bw = 1e32
    d.cols.not_nil!.x.each do |c|
      c.splits(rs, @score).each do |sp|
        w = sp.lhs.n * sp.lhs.spread + sp.rhs.n * sp.rhs.spread
        if w < bw && {sp.left.size, sp.right.size}.min >= The.leaf
          best, bw = sp, w end end end
    if (b = best)
      @col = b.col
      @cut = b.cut
      @at  = b.col.at
      @left  = Tree.new(@score).build(d, b.left)
      @right = Tree.new(@score).build(d, b.right) end
    self end

  def leaf(row : Row) : Tree
    return self if @col.nil?
    v = row[@at]
    return @left.not_nil!.leaf(row) if v == "?"
    ok = (@col.is_a?(Num) ? v.as(Float64) <= @cut.as(Float64) : v == @cut)
    (ok ? @left.not_nil! : @right.not_nil!).leaf(row) end

  def show(lvl = 0, pre = "")
    s = lvl > 0 ? "|   " * (lvl - 1) + pre : ""
    mstr = "{" + @mids.map { |k, v| "#{k}=#{Ezr.o(v)}" }.join(", ") + "}"
    printf("%-*s ,%5.2f ,(%3d),  %s\n", The.show, s, @y.mu, @y.n, mstr)
    return if @col.nil?
    c = @col.not_nil!
    ys = c.is_a?(Num) ? "<=" : "=="
    ns = c.is_a?(Num) ? ">"  : "!="
    kids = [{@left.not_nil!, ys}, {@right.not_nil!, ns}].sort_by { |k| k[0].y.mu }
    kids.each { |k| k[0].show(lvl + 1, "#{c.txt} #{k[1]} #{Ezr.o(@cut)}") } end end

# ## lib + active ----------------------------------------------
module Ezr
  extend self

  def split(col : Col, rs : Array(Row), fn : Row -> Float64, cut : Val, &test : Val -> Bool) : Split?
    lhs = Num.new; rhs = Num.new
    l  = [] of Row; r = [] of Row
    rs.each do |row|
      v = row[col.at]
      ok = v == "?" ? true : test.call(v)
      (ok ? l : r) << row
      (ok ? lhs : rhs).add(fn.call(row)) end
    l.size >= The.leaf && r.size >= The.leaf ? Split.new(col, cut, l, r, lhs, rhs) : nil end

  def csv_each(path : String, &block : Row ->)
    File.each_line(path) do |line|
      row = [] of Val
      line.split(",").each do |s|
        t = s.strip
        next if t.empty?
        v : Val = (t.to_f? || t)
        row << v end
      RNG.next_id
      block.call(row) unless row.empty? end end

  def o(x : Val) : String
    x.is_a?(Float64) ? sprintf("%.2f", x) : x.to_s end

  def thing(s : String) : Val
    s.to_f? || s end

  def shuffle!(a : Array(Row))
    (a.size - 1).downto(1) do |i|
      j = RNG.get.rand(i + 1)
      a[i], a[j] = a[j], a[i] end
    a end

  def many(a : Array(Row), n : Int32) : Array(Row)
    a2 = a.dup
    shuffle!(a2)
    a2[0, {n, a2.size}.min] end

  def wins(d : Data) : Row -> Float64
    ys = d.rows.map { |r| d.disty(r) }.sort
    lo, md = ys.first, ys[ys.size // 2]
    ->(r : Row) { (100.0 * (1.0 - (d.disty(r) - lo) / (md - lo + 1e-32))).floor } end

  def closer(lab : Data, best : Data, rest : Data, r : Row, t : Float64) : Bool
    lab.distx(r, best.mid) < lab.distx(r, rest.mid) end

  def rebalance(best : Data, rest : Data, lab : Data)
    if best.rows.size > Math.sqrt(lab.rows.size.to_f)
      bad  = best.rows.first
      badD = -1e32
      best.rows.each do |r|
        d = lab.disty(r)
        if d > badD; bad, badD = r, d; end end
      best.sub(bad); rest.add(bad) end end

  def active(rs : Array(Row), data : Data) : {Data, Data, Int32}
    lab, best, rest = data.clone_with, data.clone_with, data.clone_with
    ys = Num.new
    n0 = {4, rs.size}.min
    (0...n0).each do |k|
      lab.add(rs[k])
      ys.add(lab.disty(rs[k])) end
    sorted = lab.rows.dup.sort_by { |r| lab.disty(r) }
    nn = {1, Math.sqrt(sorted.size.to_f).to_i}.max
    sorted[0, nn].each { |r| best.add(r) }
    sorted[nn..-1].each { |r| rest.add(r) }
    k = 4
    while k < rs.size && k < The.few
      break if ys.n >= The.budget
      t = ys.n / The.budget.to_f
      if closer(lab, best, rest, rs[k], t)
        ys.add(lab.disty(rs[k]))
        lab.add(rs[k]); best.add(rs[k])
        rebalance(best, rest, lab) end
      k += 1 end
    {best, lab, ys.n} end

  def validate(rs : Array(Row), d : Data, win : Row -> Float64) : {Float64, Float64, Int32}
    shuffled = rs.dup; shuffle!(shuffled)
    n = shuffled.size // 2
    train = shuffled[0, n]
    test  = shuffled[n..-1]
    _, lab, lbl = active(train, d)
    tb    = lab.rows.first
    tbD   = 1e32
    lab.rows.each do |r|
      di = d.disty(r)
      if di < tbD; tb, tbD = r, di; end end
    tr = Tree.new(->(r : Row) { lab.disty(r) }).build(lab, lab.rows)
    test.sort_by! { |r| tr.leaf(r).y.mu }
    top = test[0, The.check].sort_by { |r| d.disty(r) }
    {win.call(tb), win.call(top.first), lbl + The.check} end end

# ## eg --------------------------------------------------------
EGS = {} of String => Proc(String, Nil)

EGS["-h"] = ->(x : String) { puts HELP; nil }

EGS["--the"] = ->(x : String) {
  puts "Budget=#{The.budget} Check=#{The.check} few=#{The.few} leaf=#{The.leaf} " \
       "p=#{The.p} seed=#{The.seed} Show=#{The.show}"
  nil
}

EGS["--csv"] = ->(f : String) {
  n = 0
  Ezr.csv_each(f) do |r|
    puts "{" + r.map { |v| Ezr.o(v) }.join(", ") + "}" if n % 30 == 0
    n += 1 end
  nil
}

EGS["--data"] = ->(f : String) {
  d = Data.new(f)
  d.cols.not_nil!.y.each { |c| puts "#{c.txt}\t#{Ezr.o(c.mid)}" }
  nil
}

EGS["--tree"] = ->(f : String) {
  d0 = Data.new(f)
  rs = Ezr.many(d0.rows, The.budget)
  d  = d0.clone_with(rs)
  Tree.new(->(r : Row) { d.disty(r) }).build(d, d.rows).show
  nil
}

EGS["--active"] = ->(f : String) {
  d = Data.new(f)
  win = Ezr.wins(d)
  20.times do
    tt, hh, nn = Ezr.validate(d.rows, d, win)
    printf("%3d %3d %3d\n", tt.to_i, hh.to_i, nn) end
  nil
}

# ## main ------------------------------------------------------
def main
  n = 0
  while n < ARGV.size
    k = ARGV[n]; n += 1
    v = n < ARGV.size ? ARGV[n] : ""
    if fn = EGS[k]?
      RNG.reseed(The.seed)
      fn.call(v)
      n += 1 unless v.empty? || EGS.has_key?(v)
    else
      case k
      when "-B" then The.budget = v.to_i; n += 1
      when "-C" then The.check  = v.to_i; n += 1
      when "-f" then The.few    = v.to_i; n += 1
      when "-l" then The.leaf   = v.to_i; n += 1
      when "-p" then The.p      = v.to_i; n += 1
      when "-s" then The.seed   = v.to_i64; RNG.reseed(The.seed); n += 1
      when "-S" then The.show   = v.to_i; n += 1 end end end end

main
