## Self initializing Variables

Write a class called Car with instance variables
wheels, color, seats.

Using the `ifNil:` method,
self initialize `wheels` at the getter level. i.e. 
when the class creates, the instance variables are nil. But
if I type

    |c|
    c := Car new.
    c  wheels oo.

then inside the `wheels` method set wheels to 4, but
only it if is currently nil.

## Iterators (1)

Go to /usr/share/gnu-smalltak/kernel and browse the
`Collection.st` class. Reflect on the methods
`reject:` and `select:` methods. 

Write your own `reject:`
method called `eject:` that does the same thing as 
`reject:` but do so calling `select`.

e.g.

     #(1 2 3) eject: [:x | x > 1.5]

     (1)

IMPORTANT: do not edit anything in that kernel directory.

## Iterators (2)

Write an iterator `b4Now:` that pass the i-th 
and (i+1)-th item to a alist:

e.g.

    #(10 21 32 43 54) b4Now: [:b4 :now|
         ((now-b4)/b4) asFloat oo] !!

    1.1
    0.5238095238095238
    0.34375
    0.2558139534883721

## Generic visit

My `Magic` class has a method called `visit` that walks
a block across all instance variables. With more
methods (not in `Magic`) such that you can write a generic
visit that walks across anything in Smalltalk (exception,
the `C*` classes that talk to ``C`` structs.

    Magic sub: #Employee has: 'name age shoesize'
    
    ! Employee methods !
    init
      self name: 'freda';
           age:  21;
           shoesize:  0 !
    
    printOn: aStream
      aStream 
         nextPutAll: 'Emp(';
         nextPutAll: ':name ',name s;
         nextPutAll: ' :age ',age s;
         nextPutAll: ' :shoesize ',shoesize s;
         nextPutAll: ')' !
    !
    
    ! Magic methods !
    visit: aBlock
      "To heck with encapulation. Walk over the instance vars."
      | num |
      num := self class instSize + self basicSize.
      1 to: num do: [:i | 
         (self instVarAt: i) visit: aBlock ] !
    !
    
    ! Object class methodsFor: 'testing' !
    goodVisit
        |x y z w|
        x := (Employee new)  name: 'tammy'.
        y := (Employee new)  name: 'tammy'.
        z := (Employee new)  name: 'Huy'; age: 18.
        w := {1. 2. #abc z. {x. x. x. {y.}.}. 4. {{{5.}.}.}.}.
        w visit:[:a| a oo] !
    !

Output:

    1
    2
    #abc
    'Huy'
    18
    0
    'tammy'
    21
    0
    'tammy'
    21
    0
    'tammy'
    21
    0
    'tammy'
    21
    0
    4
    5    
        
Challenge: make sure `#abc` is printed as `#abc` and not:

    $a
    $b
    $c

## Extra credit: max 2 marks.

For each of the following, include a litte test script showing off what can be done.


### Extra 1 (0.5)

My file my.st hows an exampe of class methods contain tests in the
category 'testing'.  Write some test cases for your classes using the
same category (and call all those methods `goodX`, just like I did). Write
Smalltalk code to

- iterate through all classes (hint `allSubclasses`)
- iterate through all methods of class methods categories called 'testing' 

### Extra 2 (0.5)

- Extra1 plus...
- run all class methods in category `testing` (hint `methodDictionary` and `perform:`)
  count how pass,fails you get

### Extra 3 (1)

- Extra2 plus...
- run methods such that if any one test method fails, the result gets recorded as "fail"
  and the test carries on to the next method. 
- Hint: see [Gnu Smaltalk manual](https://www.gnu.org/software/smalltalk/manual/html_node/Handling-exceptions.html),
