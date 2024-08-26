import strutils #, sequtils
# proc fib(n: int): int = 
#   if n <= 1: n else: fib(n - 1) + fib(n - 1)

# #for i in 0..10: echo i, " ",fib(i)

# type Animal = object
#        name: string  = "bob"
#        age: int = 23

# var dog = Animal(age: 2)  # 22

# echo dog
 
# for line in lines "fib.nim":
#   echo line.split(",").mapIt(strip(it))

type Sym = object
  at: int
  txt: string

type Num = object
  at: int
  txt: string

method add(self:Num; x:string) = add(self, parseFloat(x))
method add(self:Num; x: float | int) = 
  echo x 
   
add(Sym(at: 2,txt: "aa"), "23")