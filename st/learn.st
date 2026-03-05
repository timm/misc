"------------------------------------------------------------------------
 CHUNK 0: Compatibility Extensions
------------------------------------------------------------------------"
"Pharo already includes isZero for Number and valuesDo: for Dictionary."
Integer compile: 'printStringBase: base [ ^self printStringBase: base ]'.

"------------------------------------------------------------------------
 CHUNK 1: Output, Assignment, Basic Types, and Constants
------------------------------------------------------------------------"
[
    | x y b |
    Transcript show: 'Hello World'; cr.
    Transcript nextPutAll: 'Hello World'; cr.
    Transcript nextPut: $A; cr.
    
    'Hello' printOn: Transcript.
    Transcript cr.

    x := 4.
    x := y := 6. 
    x := (y := 6) + 1.
    x := Object new.

    b := true.
    b := false.
    x := nil.
    x := 1.
    x := 3.14.
    x := 2e-2.
    x := 16r0F.
    x := -1.
    x := 'Hello'.
    x := 'I''m here'.
    x := $A.
    x := $ .
    x := #aSymbol.
    x := #(3 2 1).
    x := #('abc' 2 $a).
] value.

"------------------------------------------------------------------------
 CHUNK 2: Booleans and Conditionals
------------------------------------------------------------------------"
[
    | x y b switch result |
    x := 1. y := 2.
    
    b := (x = y).
    b := (x ~= y).
    b := (x == y).
    b := (x ~~ y).
    b := (x > y).
    b := (x < y).
    b := (x >= y).
    b := (x <= y).
    b := b not.
    
    "In Pharo, logical AND/OR on booleans is & and |"
    b := (x < 5) & (y > 1).
    b := (x < 5) | (y > 1).
    
    "Short-circuiting"
    b := (x < 5) and: [y > 1].
    b := (x < 5) or: [y > 1].
    
    b := 123 isKindOf: Number.
    b := 123 isMemberOf: SmallInteger.
    b := x isNil.
    b := x isZero.
    b := x positive.
    b := x negative.
    b := x even.
    b := x odd.

    x > 10 ifTrue: [Transcript show: 'ifTrue'; cr] ifFalse: [Transcript show: 'ifFalse'; cr].
    
    switch := Dictionary new.
    switch at: $A put: [Transcript show: 'Case A'; cr].
    switch at: $B put: [Transcript show: 'Case B'; cr].
    result := (switch at: $B) value.
] value.

"------------------------------------------------------------------------
 CHUNK 3: Arithmetic and Bitwise Operations
------------------------------------------------------------------------"
[
    | x b rng |
    x := 6 + 3.
    x := 1 + (2 * 3).
    x := 5 / 3.      "Fraction"
    x := 5.0 // 3.0. "Integer division"
    x := 5.0 \\ 3.0. "Modulo"
    
    x := 25 sqrt.
    x := 5 raisedTo: 2.
    x := -5 abs.
    
    x := 3.5 rounded.
    x := 3.5 truncated.
    
    x := 28 gcd: 12.
    x := 10 max: 20.
    x := Float pi.
    
    rng := Random new.
    x := rng next.
    x := rng nextInt: 100. "Pharo equivalent of between:and:"

    x := 16rFF bitAnd: 16r0F.
    x := 16rF0 bitOr: 16r0F.
    x := 16rFF bitXor: 16r0F.
    x := 16r0F bitShift: 4.
] value.

"------------------------------------------------------------------------
 CHUNK 4: Conversion, Blocks, Method Calls, Characters
------------------------------------------------------------------------"
[
    | x y |
    x := 3.5 asInteger.
    x := 3 asFloat.
    x := 65 asCharacter.
    x := $A asciiValue.
    x := 15 printStringBase: 16.

    x := [ y := 1. y + 1 ].
    x value.
    
    x := [ :arg | | local | local := arg + 1. local ].

    x := 2 perform: #sqrt.

    x := $A.
    y := x isUppercase.
    y := x isDigit.
    y := x asLowercase.
] value.

"------------------------------------------------------------------------
 CHUNK 5: Iteration and Collections
------------------------------------------------------------------------"
[
    | x y sum max dict |
    x := 4. y := 1.
    [x > 0] whileTrue: [x := x - 1. y := y * 2].
    
    x := 5.
    x timesRepeat: [y := y * 2].
    1 to: 5 do: [:a | y := y * 2].

    dict := Dictionary new.
    dict at: #Key1 put: 'Value1'.
    dict keysAndValuesDo: [:k :v | Transcript show: k s, '->', v s; cr].

    x := #(1 2 3 4 5).
    y := x select: [:a | a > 2].
    y := x collect: [:a | a + a].
    
    sum := x inject: 0 into: [:a :c | a + c].
    max := x inject: 0 into: [:a :c | (a > c) ifTrue: [a] ifFalse: [c]].
] value.

"------------------------------------------------------------------------
 CHUNK 6: Streams, Files, and Reflection
------------------------------------------------------------------------"
[
    | ios x b |
    "String Streams"
    ios := ReadStream on: 'Hello read stream'.
    [ios atEnd] whileFalse: [Transcript nextPut: ios next].
    Transcript cr.

    "Modern Pharo File I/O"
    ios := 'ios.txt' asFileReference writeStream.
    ios nextPutAll: 'Hello File'; cr.
    ios close.

    ios := 'ios.txt' asFileReference readStream.
    [ios atEnd] whileFalse: [Transcript show: ios nextLine; cr].
    ios close.

    "Reflection"
    x := String allSubclasses.
    b := String instSize.
] value.

"------------------------------------------------------------------------
 CHUNK 7: Subclassing & Dynamic Method Calls
------------------------------------------------------------------------"
"Pharo 11/12 Fluid Class Definition"
(Object << #Emp
    slots: #(name role);
    package: 'Script-DynamicClasses') install.

!Emp class methods!
name: aName role: aRole
    ^self new initName: aName role: aRole!!

!Emp methods!
initName: aName role: aRole
    name := aName.
    role := aRole.
    ^self!

name ^name!
role ^role!

printOn: aStream
    aStream nextPutAll: 'Employee[Name: ', name, ', Role: ', role, ']'!!

"Execution"
[
    | argument1 argument2 receiver result worker |
    receiver := 12.
    argument1 := 10.
    argument2 := 20.
    
    result := receiver 
        perform: #between:and: 
        withArguments: {argument1. argument2}.

    worker := (Smalltalk at: #Emp) name: 'Ada Lovelace' role: 'Lead Programmer'.
    Transcript show: worker printString; cr.
] value.

"Clean termination for CLI"
Smalltalk snapshot: false andQuit: true.

