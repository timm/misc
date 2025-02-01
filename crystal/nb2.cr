alias F64 = Float64
alias Str = String

class Num
  property n = 0, mu = 0.0, m2 = 0.0

  def add(x : F64)
    @n += 1
    delta = x - @mu
    @mu += delta / @n
    @m2 += delta * (x - @mu) end

  def like(x : F64, m : F64 = 1.0)
    std = Math.sqrt(@m2 / (@n - 1)) if @n > 1
    return 1.0 if std.nil? || std == 0.0
    Math.exp(-0.5*((x - @mu)**2 / (std**2 + m**2))) / (std*Math.sqrt(2*Math::PI)) end
end

class Sym
  property n = 0, counts = Hash(Str, Int32).new(0)

  def add(x : Str) @n += 1; @counts[x] += 1 end
  def like(x : Str, m : F64 = 1.0) (@counts[x].to_f+m) / (@n + m*@counts.size) end
end

class Cols
  property x = [] of Num | Sym

  def initialize(headers : Array(Str))
    @x = headers.map { |h| h[0].upcase == h[0] ? Num.new : Sym.new } end

  def add(row : Array(Str))
    row.each_with_index do |val, i|
      col = @x[i]
      col.add(val.to_f) if col.is_a?(Num) && val.to_f?
      col.add(val) if col.is_a?(Sym) end end 
end

class Data
  property rows = [] of Array(Str)
  property cols : Cols

  def initialize(file : Str)
    @cols = Cols.new([] of Str)  # Ensure cols is initialized
    first = true
    File.open(file).each_line do |line|
      row = line.chomp.split(',')
      if first
        @cols = Cols.new(row)
        first = false
      else
        @rows << row
        @cols.add(row) end end end 
end

# Example usage:
data = Data.new("data.csv")
puts data.cols.inspect

