# 1b (Smalltalk).

## Crash Course in Smalltalk

To understand Smalltalk,reflect on

- self  =pointer to self
- super =pointer to superclass
- true  =Only intance of class True
- false  =Only intance of class False
- nil    =Only instance of class UndefinedObject
- Smalltalk =global, holds all globals; e.g. classes
- $x =character
- "  =comments
- ' =strings
     - BTW, strings are Collections of characters
- . =end statement
- ^ =return
- := =assignment
- [] =block
    - basis of all control, loops
    - [e.g whileTrue:](https://github.com/gnu-smalltalk/smalltalk/blob/master/kernel/BlkClosure.st#L346-L354)
- #() =array
- : =keyword argument
    - `5 between: 3 and: 8`  = selector `between:and:`
- Novel precedence: unary, binary, keywords, brackets
    - _Binary are one,two letter selectors for maths,
      equality, etc.
    - So `1 + 8/4` is... surprising
- Postfix messages
    -  1 negated
    - -1 negated
    -  false not
    -  false not not
    -  -1 abs
    -  1 abs
    -  10 factorial
    -  10 factorial sqrt
    -  5 sqrt
    -  1 isNumber
    -  $a isNumber
    -  $a isNumber not
    -  1 isCharacter
    -  $a isCharacter
    -  'someString' first
    -  'hello world' size
    -  'hello world' asUppercase
    -  'hello world' copy
    -  'hello world' copy sort
    -  #( 17 99 1 57 13) copy sort
    -  1 class name
    -  1 class name asUppercase
- Blocks. `aBlock value: x`.
     - e.g. `1 to: 10 by:2 do: [:x| x oo]`
     - [Number.st](https://github.com/gnu-smalltalk/smalltalk/blob/master/kernel/Number.st#L1030-L1048)
- Polymorphism: decentralized control
    - e.g. [ifTrue:](https://github.com/gnu-smalltalk/smalltalk/blob/master/kernel/True.st#L60-L71)

E.g.

    $ cd /usr/share/gnu-smalltalk/kernel/
    $ grep  '^    =='  *.st
    Object.st:    == arg [
    SmallInt.st:    == arg [
    
    $ grep '^    = ' *.st
    AnsiDates.st:    = aDateTime [
    Association.st:    = anAssociation [
    Bag.st:    = aBag [
    BindingDict.st:    = arg [
    ByteArray.st:    = aCollection [
    Character.st:    = char [
    CharArray.st:    = aString [
    Class.st:    = aClass [
    CObject.st:    = anObject [
    CompildCode.st:    = aMethod [
    CompildMeth.st:    = aMethod [
    CompiledBlk.st:    = aMethod [
    CType.st:    = anObject [
    CType.st:    = anObject [
    CType.st:    = anObject [
    Date.st:    = aDate [
    Delay.st:    = aDelay [
    Dictionary.st:    = aDictionary [
    ExcHandling.st:    = anObject [
    FileSegment.st:    = aFileSegment [
    File.st:    = aFile [
    FloatD.st:    = arg [
    FloatE.st:    = arg [
    FloatQ.st:    = arg [
    Fraction.st:    = arg [
    HashedColl.st:    = aHashedCollection [
    Interval.st:    = anInterval [
    LargeInt.st:    = aNumber [
    LookupKey.st:    = aLookupKey [
    Magnitude.st:    = aMagnitude [
    MethodInfo.st:    = aMethodInfo [
    Object.st:    = arg [
    OtherArrays.st:    = anObject [
    OtherArrays.st:    = aLargeArray [
    Point.st:    = aPoint [
    Rectangle.st:    = aRectangle [
    RunArray.st:    = anObject [
    ScaledDec.st:    = arg [
    SeqCollect.st:    = aCollection [
    SmallInt.st:    = arg [
    String.st:    = aCollection [
    Symbol.st:    = aSymbol [
    Time.st:    = aTime [
    URL.st:    = anURL [
    VFS.st:    = aFile [
    VFS.st:    = aFile [


E.g. [Point =](https://github.com/gnu-smalltalk/smalltalk/blob/master/kernel/Point.st#L198-L203) 

Everything is an object, even a class.

- Point is an instance of `Class`
- Point class methods are managers of Point instances
    - e.g. Point `x: 10 y:10`
    - class message to Point that returns an instance
      with `x=10,y= 20`

After that, you got to understand the class Hierarchy.
Literally, everything is an object.
    
    Object
        Behavior
          ClassDescription
            Class -- and Class is instance of Metaclass
            Metaclass
        BlockClosure -- []
        Boolean
          False -- true
          True  -- false
        CObject
          -- C stuff
        Collection
          Bag
          MappedCollection
          SequenceableCollection
            ArrayedCollection
              Array
              Interval
              CharacterArray
                String
                  Symbol
            LinkedList
              Semaphore
            OrderedCollection
              SortedCollection
          HashedCollection
            Dictionary
              IdentityDictionary
              RootNamespace
                SystemDictionary
            Set
              IdentitySet
        File
          Directory
        Magnitude
          Association
          Character
          Date
          Number
            Float
            Fraction
            Integer
              SmallInteger
          Time
        Message
          DirectedMessage
        Point
        Rectangle
        Signal -- exception handling. see on:do:
          Exception
            Error
              Halt
                ArithmeticError
                  ZeroDivide
                MessageNotUnderstood
              UserBreak
            Notification
              Warning
        Stream
          PositionableStream
            ReadStream
            WriteStream
              ReadWriteStream -- why not under ReadStream?
                ByteStream
                  FileStream
          Random
        UndefinedObject -- nil
   
\* [on:do:](https://www.gnu.org/software/smalltalk/manual/html_node/Handling-exceptions.html#Handling-exceptions)
 
## 1b1. Number collector (0.5 marks)
    
Write a `Magic` subclass called `Num` that is
equivalent to the following.

     function num(txt)  
         return {n=0, mu=0, m2=0, sd=0, id = id(), 
                 lo=10^32, hi=-1*10^32, txt=txt,
                 w=1}
     end

     function numInc(t,x,    d) 
       if x == "?" then return x end
       t.n  = t.n + 1
       d    = x - t.mu
       t.mu = t.mu + d/t.n
       t.m2 = t.m2 + d*(x - t.mu)
       if x > t.hi then t.hi = x end
       if x < t.lo then t.lo = x end
       if (t.n>=2) then 
         t.sd = (t.m2/(t.n - 1 + 10^-32))^0.5 end
       return x  
     end
 
     function numDec(t,x,    d) 
       if (x == "?") then return x end
       if (t.n == 1) then return x end
       t.n  = t.n - 1
       d    = x - t.mu
       t.mu = t.mu - d/t.n
       t.m2 = t.m2 - d*(x- t.mu)
       if (t.n>=2) then
         t.sd = (t.m2/(t.n - 1 + 10^-32))^0.5 end
       return x
     end
 
When that works, it should do the following:

     | num |
     num := Num new.
     num nextPutAll: #( 2 3 4 4 4 4 5 5 6 7 7
                      8 9 9 9 9 10 11 12 12).
     num sd oo. "==> 3.06"
     num mu oo. "==> 7"
     num n  oo. "==> 20"

## 1b2. Iterators (0.5 marks)

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

HINT: the solution is two lines long.

## 1b3. Iterators (0.5 marks)

Write an iterator `b4Now:` that pass the i-th 
and (i+1)-th item to a alist:

e.g.

    #(10 21 32 43 54) b4Now: [:b4 :now|
         ((now-b4)/b4) asFloat oo] !!
    
    1.1
    0.5238095238095238
    0.34375
    0.2558139534883721

## Generic visit (1.5 marks)

My `Magic` class has a method called `visit` that walks
a block across all instance variables. Write more
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
        
Important: make sure `#abc` is printed as `#abc` and not:

    $a
    $b
    $c

## Extra credit: max 1 marks.

For each of the following, include a litte test script showing off what can be done.



My file my.st hows an exampe of class methods contain tests in the
category 'testing'.  Write some test cases for your classes using the
same category (and call all those methods `goodX`, just like I did). Write
Smalltalk code to

- iterate through all classes (hint `allSubclasses`)
- iterate through all methods of class methods categories called 'testing' 
- run all class methods in category `testing` (hint `methodDictionary` and `perform:`)
  count how pass,fails you get
- run methods such that if any one test method fails, the result gets recorded as "fail"
  and the test carries on to the next method. 
- Hint: see [Gnu Smaltalk manual](https://www.gnu.org/software/smalltalk/manual/html_node/Handling-exceptions.html)
      and `on:do:`.
