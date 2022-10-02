<<<<<<< HEAD
# ime julia -O0 --compile=min --startup=no aa.jl
# using Pkg
=======
# using Pkg
# Pkg.add(["Printf", "ResumableFunctions","Parameters", "Random" ])
#
# tryusing(pkgsym) = try
#     @eval using $pkgsym
#     return true
# catch e
#     Pkg.add(pkgsym)
# end
# tryusing(:Printf);tryusing(:ResumableFunctions); tryusing("Parameters"); tryusing("Random")
#

>>>>>>> 925713e0671fa8b996b6bd98682a578f82037ffd
using Printf, ResumableFunctions,Parameters,Random
# function to calculate the volume of a sphere
function sphere_vol(r)
    # julia allows Unicode names (in UTF-8 encoding)
    # so either "pi" or the symbol π can be used
    return 4/3*pi*r^3
end

# functions can also be defined more succinctly
quadratic(a, sqr_term, b) = (-b + sqr_term) / 2a

# calculates x for 0 = a*x^2+b*x+c, arguments types can be defined in function definitions
function quadratic2(a::Float64, b::Float64, c::Float64)
    # unlike other languages 2a is equivalent to 2*a
    # a^2 is used instead of a**2 or pow(a,2)
    sqr_term = sqrt(b^2-4a*c)
    r1 = quadratic(a, sqr_term, b)
    r2 = quadratic(a, -sqr_term, b)
    # multiple values can be returned from a function using tuples
    # if the return keyword is omitted, the last term is returned
    r1, r2
end

vol = sphere_vol(10)
# @printf allows number formatting but does not automatically append the \n to statements, see below
using Printf
@printf "volume = %0.3f\n" vol 
#> volume = 113.097

quad1, quad2 = quadratic2(10.0, -2.0, -12.0)
println("result 1: ", quad1)
#> result 1: 3.0
println("result 2:: ", quad2)


@resumable function fibonacci(n::Int) :: Int
	a,b = 0,1
	for i in 1:n
		@yield a
		a, b = b, a+b; end end

for fib in fibonacci(100)
	println(fib)
end

@with_kw mutable struct Config
  char = (skip='?',less='>',more='<',num='$',klass='!')
  str  = (skip="?",)
  some = (max=512,step=.5, cohen=.3, trivial=1.05)
  seed = 1
end

THE = Config()
Random.seed!(THE.seed)
print(rand())
