class NaiveBayes
  @data : Array(Array(Float64))
  @labels : Array(String)
  @nClass : Hash(String, Int32)
  @fCounts : Hash(Tuple(String, Int32), Float64)

  def initialize(data : Array(Array(Number)), labels : Array(String))
    @data = data.map { |row| row.map(&.to_f).to_a }  # Explicitly create Array(Float64)
    @labels = labels
    @nClass = {} of String => Int32
    @fCounts = {} of Tuple(String, Int32) => Float64

    # Calculate class and feature counts
    labels.each_with_index do |label, i|
      @nClass[label] ||= 0
      @nClass[label] += 1

      @data[i].each_with_index do |value, j|
        @fCounts[{label, j}] ||= 0.0
        @fCounts[{label, j}] += value end end end

  def predict(sample : Array(Float64))
    probs = {} of String => Float64
    @nClass.keys.each do |label|
      probs[label] = Math.log(@nClass[label].to_f / @labels.size) end

    sample.each_with_index do |value, feature_index|
      @nClass.keys.each do |label|
        fCount = @fCounts[{label, feature_index}] || 0.0
        probs[label] += Math.log((fCount + 1.0) / (@nClass[label] + sample.size).to_f) end end

    probs.max_by { |_, prob| prob }[0] end end

# Example usage
data = [
  [1, 0, 1],
  [0, 1, 0],
  [1, 1, 1]
].map { |row| row.map(&.to_f).to_a }  # Force Array(Array(Float64))

labels = ["positive", "negative", "positive"]

model = NaiveBayes.new(data, labels)
prediction = model.predict([1.0, 0.0, 0.0])
puts "Prediction: #{prediction}"

