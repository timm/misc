package main

import "fmt"

type f struct {
  a,b,c int
}

func (x f) add() int {
  return x.a + x.b + x.c
}

func main() {
  fmt.Println(f{a:1, c:4}.add())
