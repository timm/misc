Transcript nextPutAll: '------------------------------------------------------------------------
 CHUNK 0: Compatibility Extensions (Fixing missing Squeak methods)
------------------------------------------------------------------------'; cr !

"Add missing isZero to Number"
Number extend [
    isZero [
        ^self = 0
    ]
] !

"Add missing valuesDo: to Dictionary (in GST, do: iterates values by default)"
Dictionary extend [
    valuesDo: aBlock [
        self do: aBlock
    ]
] !

"Add missing printStringBase: to Integer (GST uses printString: instead)"
Integer extend [
    printStringBase: base [
        ^self printString: base
    ]
] !

Transcript nextPutAll: '------------------------------------------------------------------------
 CHUNK 1: Output, Assignment, Basic Types, and Constants
------------------------------------------------------------------------'; cr !

Eval [
    | x y b |
    Transcript show: 'Hello World'.
    Transcript nextPutAll: 'Hello World'.
    Transcript nextPut: $A.
    Transcript space; tab; cr.
    'Hello' printOn: Transcript.
    'Hello' storeOn: Transcript.
    Transcript endEntry.

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
] !

Transcript nextPutAll: '------------------------------------------------------------------------
 CHUNK 2: Booleans and Conditionals
------------------------------------------------------------------------'; cr !

Eval [
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
    b := (x < 5) & (y > 1).
    b := (x < 5) | (y > 1).
    b := (x < 5) and: [y > 1].
    b := (x < 5) or: [y > 1].
    b := (x < 5) eqv: (y > 1).
    b := (x < 5) xor: (y > 1).
    b := 5 between: 3 and: 12.
    
    b := 123 isKindOf: Number.
    b := 123 isMemberOf: SmallInteger.
    b := x isNil.
    b := x isZero. "<- Works now thanks to Chunk 0"
    b := x positive.
    b := x strictlyPositive.
    b := x negative.
    b := x even.
    b := x odd.
    b := x isInteger.
    b := x isFloat.
    b := x isNumber.
    b := $A isUppercase.
    b := $A isLowercase.

    x > 10 ifTrue: [Transcript show: 'ifTrue'; cr].
    x > 10 ifFalse: [Transcript show: 'ifFalse'; cr].
    x > 10 ifTrue: [Transcript show: 'ifTrue'; cr] ifFalse: [Transcript show: 'ifFalse'; cr].
    x > 10 ifFalse: [Transcript show: 'ifFalse'; cr] ifTrue: [Transcript show: 'ifTrue'; cr].
    
    switch := Dictionary new.
    switch at: $A put: [Transcript show: 'Case A'; cr].
    switch at: $B put: [Transcript show: 'Case B'; cr].
    switch at: $C put: [Transcript show: 'Case C'; cr].
    result := (switch at: $B) value.
] !

Transcript nextPutAll: '------------------------------------------------------------------------
 CHUNK 3: Arithmetic and Bitwise Operations
