package main

import ( "fmt";"time")

var s=fmt.Println
func main() {

	i := 3
	fmt.Print("Write ", i, " as ")
	switch i {
	case 1: s("one")
	case 2: s("two");s("five")
	case 3: s("three") }

	switch time.Now().Weekday() {
	case time.Saturday, time.Sunday:
		s("It's the weekend")
	default: s("It's a weekday") }

	t := time.Now() 
	switch {
	case t.Hour() < 12: s("It's before noon")
	default: s("It's after noon") }

	whatAmI := func(i interface{}) {
		switch t := i.(type) {
		case bool: s("I'm a bool");s("I'm a bool")
		case int: s("I'm an int")
		default: s("Don't know type %T\n", t) } }
	whatAmI(true)
	whatAmI(1)
	whatAmI("hey")
}
