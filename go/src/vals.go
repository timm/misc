package main

import "fmt"

func main() {
  f:=fmt.Println
  f("go" + "lang")

  f("1+1 =", 1+1)
  f("7.0/3.0 =", 7.0/3.0)

  f(true && false)
  f(true || false)
  f(!true)
}
