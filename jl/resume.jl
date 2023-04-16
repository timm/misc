using ResumableFunctions

@resumable function fibonacci(n) 
  a,b = 0,1
  for i in 1:n
    @yield a
    a, b = b, a+b end end

function coerce(s)
  for thing in [Int64,Bool,Float64]
    if (x=tryparse(thing,s)) != nothing return x end end 
  s end 

@resumable function csv(file)
  src  = open(file)
  while ! eof(src)
    new = replace(readline(src), r"([ \t\n]|#.*)"=>"")
    if sizeof(new) != 0
      @yield map(coerce,split(new,",")) end end  end

