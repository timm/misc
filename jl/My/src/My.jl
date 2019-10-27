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

@with_kw struct Lines file; src=open(file) end

"Define an iterator that returns a comma-seperated file, one
 record at a time without loading the whole file into memory."
function Base.iterate(it::Lines, (n,want)=(1,[]))
  "Split on comma, coerce strings to numbers or strings, as approriate." 
  coerce(s)  = map(coerce1, split(s,","))
  coerce1(s) = ((x = tryparse(Float64,s))==nothing) ? s : x
  use(s)     = match(r"\?",s) == nothing

  "Coerce strings. If first row, check what columns we should use.
   Only return those columns."
  function cols(lst)
    if n == 1 
      want = [i for (i,s) in enumerate(lst) if use(s)] end
    [lst[i] for i in want]  end 
  
  "Delete comments and whitespace. Lines ending in
   ',' are joined to the next. Skip empty lines."
  function row(txt="")
    while true
      if eof(it.src) return txt end
      new = readline(it.src)
      new = replace(new, r"([ \t\n]|#.*)"=>"")
      if sizeof(new) != 0 
        txt *= new
        if txt[end] != ',' 
	  return txt end end end end 

  new = row()
  if sizeof(new) > 0 
    (n, cols(coerce(new))) , (n+1,want) end 
 end

function xomo()
  m=1
  print(m)
  for (n,tmp) in Lines(file= "data/xomo10000.csv")
     m += sizeof(tmp)  #println(n," ",tmp)
     if mod(n,1000) == 0 println(n,":",m) end
  end
  print(m)
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
