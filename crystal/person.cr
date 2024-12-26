class Person
  @job=["manager"]
  @age=20
  def name(s : String) @name = s; self end
    def job(s : String) @job[0]=s end
  def age()    @age end
  def older()  @age += 1 end end

john = Person.new  
peter = Person.new.name "Peter"

peter.older

puts peter.@job
puts peter.@age
peter.job "cleaner"
puts peter.@job
puts john.@job
puts john.@age

puts typeof(peter)

class Example
  def initialize
    @var1 = "Hello"
    @var2 = 123
    @var3 = [1, 2, 3]
  end

  def loops
    self.instance_variables.each do |var|
      value = self.instance_variable_get(var)
      puts "#{var} = #{value}"
    end
  end
end

obj = Example.new
obj.loops


# john.name(32)
# puts john.@name 
#
# john.older
# puts john.age # => 1
#
# puts peter.age # => 0
#
# struct Int
#   def tomes(&)
#     i = 0
#     while i < self
#       yield i
#       i += 1 end end end
#
# 3.tomes do  |i|
#   puts i 
# end 
#
class String
  def sss() self.size end end

puts "asdasa".size
puts "asdasa".sss
