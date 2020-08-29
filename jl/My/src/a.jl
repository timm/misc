using Parameters
using Random

@with_kw mutable struct Config
  char = (skip='?',less='>',more='<',num='$',klass='!')
  str  = (skip=",")
  some = (max=512)
  seed = 1
end

my=Config()
#Random.seed!(my.seed)

same(s) = s
int(x)  = floor(Int,x)
any(a)  = a[ int(size(a)[1] * rand()) + 1]

function say(i)
  s,pre="$(typeof(i)){",""
  for f in sort!([x for x in fieldnames(typeof(i)) 
                     if !("$x"[1] == '_')])
    g = getfield(i,f)
    s = s * pre * "$f=$g" 
    pre=", "
  end
  print(s * "}")
end

adds!(init=[],i=Some) = incs!(i(),init, 1)
subs!(init=[],i=Some) = incs!(i(),init,-1)
add!(i,x)             = inc!( i  ,   x, 1)
sub!(i,x)             = inc!( i  ,   x,-1)
incs!(i,init=[],w=1)  = begin [inc!(i,x,w) for x in init]; i end


main() = for _ in 1:10; print(any([1,2,3])) end

try
  println("\n--| ",int(time() % 10000)," |----------------")
  main()
catch e
   showerror(stdout, e, catch_backtrace()[1:10])
end
