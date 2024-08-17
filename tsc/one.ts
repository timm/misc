function greet(name: string): string { return `Hello, ${name}!` }

let say=console.log
let person: { [key: string]: string }  ={}
person["name"] = "Alice";
person["age"] = "30";

say(person)
say(greet("asdas"))


class Person {
  name: string; age: number;

  constructor(name: string, age: number) { this.name = name; this.age = age; }

  greet():  {
    console.log(`Hello, my name is ${this.name} and I am ${this.age} years old.`);
  }
}


const person1 = new Person("Alice", 30);
person1.greet()

