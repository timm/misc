FileStream fileIn: '0lib.st' !

'a list of words' asWords oo ! 

"Smalltalk so verbose

OrderedCollection... why not List?"

ObjectMemory halt !

|g r usage s|
usage := '-e|--eg:i -a|--all'.

Smalltalk 
  arguments:  usage
  do: [ :opt :arg |
        opt = 'all' ifTrue: []

        opt = 'eg'      ifTrue: [
          g := Graph new.
          arg oo.
          s := 'eg/', arg s.
          s oo.
          r := Reader new lines: ('eg/',arg ) ofor: g.
          g width oo.
        ]
  ]
  ifError: [ ('usage: ', usage) oo ]
!
" cheat sheethrome-extension://nkoccljplnhpfnfiajclkommnmllphnl/html/crosh.html"
"see /usr/share/gnu-smalltalk/kernel for ad"

"Meta programming"

2 perform: #+ with: 2.


" default values suck"
" Have I seen this before?
Enter Identity dicaionaries"

"Equality. when do two things ahve the same val

cd src
grep '^    ==' *.st
Object.st:    == arg 
SmallInt.st:    == arg 

grep '^    = ' *.st | wc # 46 replies. 

See Point = and SeqCollect 
"
|seen|
seen := IdentityDictionary new.
y := #(1 2 3)
x := #(1 2 3)
seen add: y;
     add: y;
     add: x;
     size.  "returns 2, not 3"
 
!Object methods! 
all: block
  ^self all:block seen: IdentitySet new depth:0 

all: block seen: y newvalue: self !!

! Collection methods!
all: block
  self do: [:x| block value: x] !!

"st broke my heart

fragmentaion f the vendor communty. limited portablility.

proinet at the brth of agile
didnt keep up: poor version control, package manage system, community of people building tools

to little encpasulation. cry

but as a palce to worship on the alter of objects...

"

"lessons having the heiratchy in your face https://www.gnu.org/software/smalltalk/manual/html_node/The-existing-hierarchy.htmlfr me was great. for others... freaky"

"inheriatance= meh"

" its all about the blocks baby"

|y|
y = [:x | x > 7]
Transcript show: (y value: 8).
Transcript show: (y value: 6).

" dont case on type. put your behaviour into the class




  Object
    Behavior
      ClassDescription
        Class
        Metaclass
    BlockClosure
    Boolean
      False
      True
    CObject
      ...
    Collection
      Bag
      SequenceableCollection
        ArrayedCollection  # directly accessible by an index
          Array
          ByteArray
          Interval
          CharacterArray
            String # array of symbols
              Symbol
        LinkedList
          Semaphore
        OrderedCollection # not directory indedex by an index
          SortedCollection # as a sideeffect of adding something, sort it into place
      HashedCollection
        Dictionary # ==> keys compred using =
          IdentityDictionary # keys compared using ==
          RootNamespace
            SystemDictionary # ==> on instance (smalltalk)
        Set # items compares using =
          IdentitySet # items compared using ==
    File
      Directory
    Magnitude # anything that can be < >
      Association
      Character
      Date
      Number
        Float
        Fraction
        Integer
          LargeInteger
          SmallInteger
      Time
    Message
    Point
    Rectangle
    Signal
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
          ReadWriteStream
            ByteStream
              FileStream
      Random
    UndefinedObject # singleeverything is an opbject. even nil
"
