
# a.jl

calculates x for 0 = a*x^2+b*x+c, [arguments types](https://docs.julialang.org/en/v1/manual/functions/#Further-Reading-1) can be defined in function definitions

```julia
function quadratic2(a,b,c) # ::Float64, b::Float64, c::Float64)
    # unlike other languages 2a is equivalent to 2*a
    # a^2 is used instead of a**2 or pow(a,2)
    sqr_term = sqrt(b^2-4a*c)
    r1 = quadratic(a, sqr_term, b)
    r2 = quadratic(a, -sqr_term, b)
    # multiple values can be returned from a function using tuples
    # if the [return](https://docs.julialang.org/en/v1/manual/functions/#The-return-Keyword-1) keyword is omitted, the last term is returned
    r1, r2
end


````

