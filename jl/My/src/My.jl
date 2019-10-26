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

@with_kw struct Lines file= ""; src= open(file) end

"Define an iterator that returns a comma-seperated file, one
 record at a time without loading the whole file into memory."
function Base.iterate(it::Lines, (n,use)=(1,[]))
  "Split on comma, coerce strings to numbers or strings, as approriate." 
  cells(s)    = map(cell, split(s,","))
  cell(s)     = ((x = tryparse(Float64,s))==nothing) ? s : x
  relevant(s) = match(r"\?",s) == nothing

  function words(txt)
      lst = cells(txt)
      println(">>",lst)
      if n == 1 
        use = [i for (i,s) in enumerate(lst) if relevant(s)]
      else
        [lst[i] for i in use]  end end
  
  "Delete comments and whitespace. Lines ending in
   ',' are joined to the next. Skip empty lines."
  function row(txt="")
    while true
      if eof(it.src) return words(txt) end
      new = readline(it.src)
      new = replace(new, r"([ \t\n]|#.*)"=>"")
      if sizeof(new) != 0 
        txt *= new
        if txt[end] != ',' 
	  return words(txt) end end end end 

  new = row()
  if sizeof(new) > 0 (n,new) , (n+1,use) end
 end 

for (n,tmp) in Lines(file= "data/weather.csv")
   println(n," ",tmp)
end

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
