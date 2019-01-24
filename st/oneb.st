#!/usr/bin/env gst

"--- timm tricks -----"

! Object methods !
s
  ^self printString !

oo
   Transcript show: self s;
              cr!!

! String methods !
asWords
  ^(self tokenize: '\s+') asArray !!

! Class methods !
public
  |getter setter|
  self instanceVariableString asWords do: [:what |
       getter := '%1 [
         "Answer the receiver''s %1."
         ^%1 ]' % {what} .
       setter := '%1: aValue [
         "Set the receiver''s %1."
          %1 := aValue ]' % {what} .
       self compile: getter;
            compile: setter.  ] !
!
! Class methods !
sub: kid  
   ^self sub: kid has: ''  shares: '' !

sub: kid has: vars 
   ^self sub: kid has: vars shares: ''!

sub: kid has: vars1 shares: vars2
   |tmp|
   tmp := self
     subclass: kid
     instanceVariableNames: vars1
     classVariableNames: vars2
     poolDictionaries: ''
     category: nil.
   tmp public.
   ^tmp !
!

Object sub: #Magic!

! Magic class methods !
new
  ^super new init !!

! Magic methods !
init !!

"============================================="

! Object methods !
visit: aBlock
  ^aBlock value: self !!

! Collection methods !
visit: aBlock
  self do: [:x|
    x visit: aBlock]!!

! String methods !
visit: aBlock
  aBlock value: self !!

! Magic methods !
visit: aBlock
  "To heck with encapulation. Walk over the instance vars."
  | num |
  num := self class instSize + self basicSize.
  1 to: num do: [:i | 
     (self instVarAt: i) visit: aBlock ]
!!

! Object methods !
hop: aBlock
  ^self hop: aBlock 
        stop: [:at :seen| self stop: at seen: seen] 
        at:  0@0 
        seen: IdentityDictionary new!!

hop: aBlock stop: aStop at: at seen: seen
  (seen includesKey: self)
     ifTrue: [^aStop value: at value: seen]
     ifFalse:[
      
  ^aBlock value: self  
          value: at 
          value: (seen includesKey: self)     

! Collection methods !
hop: aBlock at: at seen: seen
  (seen includes: self) 

  self do: [:x|
    x hop: aBlock at: at+(1@1) ]!!

! String methods !
visit: aBlock
  aBlock value: self !!

! Magic methods !
visit: aBlock
  "To heck with encapulation. Walk over the instance vars."
  | num |
  num := self class instSize + self basicSize.
  1 to: num do: [:i | 
     (self instVarAt: i) visit: aBlock ]
!!
        
"===| Employee |=============================="
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

|x y z all|

x := (Employee new)  name: 'tammy'.
y := (Employee new)  name: 'tammy'.
z := {1. 2. #b. 3. {x. x. x. {y.}.}. 4. {{{5.}.}.}.}.
"z visit: [:n| n oo].
"
(z at: 5) visit: [:x | x oo].

1
