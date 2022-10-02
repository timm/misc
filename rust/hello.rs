#!/usr/bin/env ./rust

// fn main1() { 
//     let x=5;
//     println!("Is `x` 10 or 100? x = {}", x);
//     println!("Hello World!"); 
//     println!("{1:?} {0:?} is the {actor:?} name.",
//                  "Slater",
//                  "Christian",
//                  actor="actor's");
//     println!("Hello World!") }
//
// fn main3() {
//     for i in 0..5 {
//         println!("Hello {}", i); } }
//
//
// fn main5() {
//     let mut v = Vec::new();
//     v.push(10);
//     v.push(20);
//     v.push(30);
//
//     let first = v[0];  // will panic if out-of-range
//     let maybe_first = v.get(0);
//
//     println!("v is {:?}", v);
//     println!("first is {}", first);
//     println!("maybe_first is {:?}", maybe_first);
// }
//
// fn maini6() {
//     for i in 0..5 {
//         if i % 2 == 0 {println!("even {}", i)} else  {println!("odd {}", i)}}}
//
//

// fn main() {
//     for i in 0..5 {
// 	    let even_odd = if i % 2 == 0 {"even"} else {"odd"};
//         println!("{} {}", even_odd, i); } }
//

//
// fn main() {
//     let mut sum = 0.0;
//     for i in 0..5 {
//         sum += i as f64;
//     }
//     println!("sum is {}", sum);
// }
//
//
// fn sqr(x: f64) -> f64 { x * x }
//
// fn fact(n: u64) -> u64 {
//     if n == 0 {1} else { n * fact(n-1)}}
//
// // absolute value of a floating-point number
// fn abs(x: f64) -> f64 {if x > 0.0 { x } else {-x}}
// fn main() {
//     println!("square is {} abs is {}", sqr(2.0),abs(-2.4));
//     for i in 1..20 {
//       println!("fact {}  is {}",i,fact(i))}
// }
fn by_ref(x: &i32) -> i32{
    *x + 1
}

fn main() {
    let i = 10;
    let res1 = by_ref(&i);
    let res2 = by_ref(&41);
    println!("{} {} {}", i,res1,res2);
}
