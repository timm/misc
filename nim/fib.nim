# asdas dasdasas
proc fib(n: int): int =
  if n <= 1: n else: fib(n - 1) + fib(n - 1)

# asdasdsa asdasda
#for i in 0..10: echo i, " ",fib(i)

type Animal = object
       name: string  = "bob"
       age: int = 23

var dog = Animal(age: 1)

echo dog