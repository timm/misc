using ResumableFunctions

@resumable function fibonacci(n::Int) :: Int
	a = 0
	b = 1
	for i in 1:n
		@yield a
		a, b = b, a+b
	end
end

for fib in fibonacci(10)
	println(fib)
end
