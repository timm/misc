module My
using Parameters 

greet() = print("Hello World!")

@with_kw mutable struct Cols
  x  = (all  = [], nums = [], syms = [])
  y = (all  = [], nums = [], syms = [],
           goals= [22], klass= nothing)
  all  = [] 
  nums = [] 
  syms = (10, 20,   30, 40)
  name= ""
  pos=0
end

z=Cols(name="jane", pos=2)
#show(z)
#w = Cols(),
#dump(w)
#dump(nothing)

function dd()
open("sherlock-holmes.txt") do f
   line = 1
   while !eof(f)
     x = readline(f)
     println("$line $x")
     line += 1
   end
 end
end

#[take(PSerie(2),10)...]
#


#print(Lines(name="src/My.jl"))

using Parameters
@with_kw mutable struct Lines
  name = ""
  src    = open(name)
end

"Define an iterator that returns a comma-seperated file, one
 record at a time without loading the whole file into memory."
function Base.iterate(z::Lines,(n)=(1))
  "Co-erce strings to numbers or strings, as approriate." 
  function atom(s)
    v = tryparse(Float64,s) 
    v == nothing ? s : v
  end
  "Delete comments and whitespace. Lines ending in
   ',' are joined to the next. Skip empty lines."
  function row(str,b4="")
    while true
      if eof(str) return b4 end
      r = replace(readline(str),r"([ \t\n]|#.*)"=>"")
      if sizeof(r) == 0 continue end
      if r[end] == ',' 
        b4 *= r
      else
        return b4 * r end end 
  end
  "Split on comma, then coerce sub-strings."
  function prep(words)
    [atom(word) for word in split(words,",")]
  end
  #------------------------------
  line = row(z.src)
  if sizeof(line) > 0 prep(line),(n+1) end
 end 

for tmp in Lines(name="data/weather.csv")
   println(tmp)
end
#
#function Base.iterate(z::Lines, (s,n) = (0,1))
#    s += 1 / (n^ps.p)
#           s, (s, n+1)
#       end
#

function demo()
  print(444)
  open("src/My.jl") do file
      n=1
      ld=""
      function prep(s)
        if sizeof(s) > 0
          println( map(num, split(s,",")))end end
      for line in eachline(file)
          line = replace(line, r"([ \t\n]|#.*)" => "")
          if sizeof(line) == 0  continue end
          print(line[end])
          if line[end] == ',' 
            old = old * line
          else
            prep(old*line)
            old = ""
          end 
      end
      prep(old)
  end
end 
end
