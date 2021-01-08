include("./a.jl")

# [function](https://docs.julialang.org/en/v1/manual/functions/#man-functions-1) to calculate the volume of a sphere
function sphere_vol(r)
    # julia allows [Unicode names](https://docs.julialang.org/en/v1/manual/unicode-input/#Unicode-Input-1) (in UTF-8 encoding)
    # so either "pi" or the symbol π can be used
    return 4/3*pi*r^3
end

# functions can also be defined more succinctly
quadratic(a, sqr_term, b) = (-b + sqr_term) / 2a

vol = sphere_vol(3)
# @printf allows number formatting but does not automatically append the \n to statements, see below
using Printf
@printf "volume = %0.3f\n" vol 
#> volume = 113.097

quad1, quad2 = quadratic2(2.0, -2.0, -12.0)
println("result 1: ", quad1)
#> result 1: 3.0
println("result 2: ", quad2)
#> result 2: -2.0
