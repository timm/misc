o = fn(x) -> IO.puts(x) end

square = fn(x) -> x * x end
square.(5) #=> 25

defmodule Nums do
  def fib(a, _, 0 ) do a end
  def fib(a, b, n) do fib(b, a+b, n-1) end
end

o.(square.(5))
o.(Nums.fib(1,1,6))

defmodule Seq do
  def sum([],n)     do n end 
  def sum([h|t], n) do sum(t, h+n) end end

o.(Seq.sum([1,2,3], 0))

Enum.each 1..40, fn c -> 
  Enum.each 1..c, fn b -> 
    Enum.each 1..b, fn a -> 
      if a * a + b * b == c * c do
        o.("#{a}, #{b}, #{c}")  end end end end

#o.(Lst@intro)
o.( if true, do: :ok, else: :error)
