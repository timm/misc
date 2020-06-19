package main

import "fmt"
var s= fmt.Println

func main() {
    var a = "initial"
    s(a)

    var b, c = 1, 2
    s(b, c)

    var d = true
    s(d)

    var e int
    s(e)

    f := "apple"
    s(f)
    i := 1
    s(i)
}
