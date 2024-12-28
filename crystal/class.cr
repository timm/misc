require "csv"

# Numeric column struct
class Num
  property column : Int32
  property txt : String
  property n : Int32 = 0
  property mu : Float64 = 0.0
  property sd : Float64 = 0.0
  property lo : Float64 = Float64::INFINITY
  property hi : Float64 = -Float64::INFINITY

  def initialize(@column : Int32, @txt : String)
  end

  def update(value : Float64?)
    return unless value

    @n += 1
    delta = value - @mu
    @mu += delta / @n
    @sd += delta * (value - @mu)
    @lo = Math.min(@lo, value)
    @hi = Math.max(@hi, value)
  end

  def likelihood(value : Float64) : Float64
    return 1.0 if @n < 2 || @sd == 0

    variance = @sd / (@n - 1)
    z = (value - @mu) / Math.sqrt(variance)
    Math.exp(-0.5 * z**2) / (Math.sqrt(2 * Math::PI * variance))
  end
end

# Symbolic column struct
class Sym
  property column : Int32
  property txt : String
  property n : Int32 = 0
  property all = Hash(String, Int32).new(0)
  property mode : String? = nil
  property most : Int32 = 0

  def initialize(@column : Int32, @txt : String)
  end

  def update(value : String?)
    return unless value

    @n += 1
    @all[value] += 1
    if @all[value] > @most
      @most = @all[value]
      @mode = value
    end
  end

  def likelihood(value : String) : Float64
    (@all[value]?.try(&.to_f64) || 0.0) / @n || 1e-9
  end
end

# Data struct to hold rows and column summaries
class Data
  property rows = [] of CSV::Row
  property cols : Hash(String, Num | Sym)

  def initialize(@cols : Hash(String, Num | Sym))
  end
end

# Initialize columns based on header row
def initialize_cols(header : Array(String))
  nums = Hash(String, Num).new
  syms = Hash(String, Sym).new
  klass = ""

  header.each_with_index do |name, i|
    if name.ends_with?("!")
      klass = name
      syms[name] = Sym.new(i, name)
    elsif name[0].ascii_uppercase?
      nums[name] = Num.new(i, name)
    else
      syms[name] = Sym.new(i, name)
    end
  end

  {nums: nums, syms: syms, klass: klass}
end

def likelihood(row : Hash(String, String), data : Data) : Float64
  likelihood = 1.0

  data.cols.each do |name, col|
    value = row[name]?
    likelihood *= case col
                  when Num
                    col.likelihood(value.try(&.to_f64) || 0.0)
                  when Sym
                    col.likelihood(value.to_s)
                  end
  end

  likelihood
end


# Process the CSV and classify rows
def read_csv(file : String)
  csv_text = File.read(file)
  rows = CSV.parse(csv_text)

  # Extract headers from the first row
  header = rows.shift.not_nil!

  cols_by_class = Hash(String, Data).new
  processed_rows = 0

  cols = initialize_cols(header)

  rows.each do |row|
    # Map each row to a hash of header => value
    mapped_row = header.zip(row).to_h

    if processed_rows >= 20
      best_class = ""
      max_likelihood = -Float64::INFINITY

      cols_by_class.each do |class_name, data|
        l = likelihood(mapped_row, data)
        if l > max_likelihood
          max_likelihood = l
          best_class = class_name
        end
      end

      puts "Row #{processed_rows + 1}: Classified as class #{best_class}"
    end

    class_value = mapped_row[cols[:klass]]?

    cols_by_class[class_value] ||= Data.new(cols[:nums].merge(cols[:syms]))

    cols_by_class[class_value].rows << mapped_row

    # Update summaries
    cols_by_class[class_value].cols.each do |name, col|
      value = mapped_row[name]?
      case col
      when Num
        col.update(value.try(&.to_f64))
      when Sym
        col.update(value)
      end
    end

    processed_rows += 1
  end

  cols_by_class
end



# Example usage
file_path = "weather.csv"  # Replace with your CSV file path
read_csv(file_path)

