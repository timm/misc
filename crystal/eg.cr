alias Cmd = Proc(Nil) | Proc(String, Nil)
EG = Hash(String, Cmd).new

class Config
  # 1. 'val' now uses Ints, Bools, and Strings
  OPTIONS = {
    :a    => {val: 1,              txt: "comment1"},
    :b    => {val: 2,              txt: "comment2"},
    :c    => {val: true,           txt: "comment3"},
    :file => {val: "auto93.csv",   txt: "comment4"} 
  }

  def self.help_string
    puts "Usage: app [args]"
    OPTIONS.each { |k, v| puts " -#{k}=#{v[:val]}\t#{v[:txt]}" }
  end

  def self.cli
    while ARGV.size > 0
      arg = ARGV.shift.lstrip('-')
      if (cmd = EG[arg]?)
        cmd.is_a?(Proc(Nil)) ? cmd.call : cmd.call(ARGV.shift? || "")
      elsif (opt = OPTIONS[k = arg.to_sym]?)
        raw = ARGV.shift? || ""
        # 2. Type casting based on the existing value's type
        typed = case opt[:val]
                when Int32 then raw.to_i
                when Bool  then raw == "true"
                else            raw
                end
        OPTIONS[k] = {val: typed, txt: opt[:txt]}
      end
    end
  end
end

# 3. No space between THE and (
def THE(k : Symbol)
  Config::OPTIONS[k][:val]
end

# Define commands
EG["a"] = ->{ puts "Direct Command A: 12" }
EG["b"] = ->(arg : String) { puts "Starting B with... #{arg}" }
EG["help"] = ->{ Config.help_string }

# Execute
Config.cli

# Now THE(:a) returns an Int32, not a String
puts "File: #{THE(:file)} (#{THE(:file).class})"
puts "A + 10: #{THE(:a).as(Int32) + 10}"

