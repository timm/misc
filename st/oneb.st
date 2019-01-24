#!/usr/bin/env gst
i
"--- timm tricks -----"

! Object methods !
s
  ^self printString !
oo
  ^Transcript show: self s; cr !!

! String methods !
asWords
      ^(self tokenize: '\s+') asArray !!

! Class methods !
inits
  self class compile: 'new [^super new init]' !

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

! Object methods !
each: aBlock
  ^aBlock value: self !!

! Collection methods !
each: aBlock
  self do: aBlock !!

! Class methods !
kid: spec
   ^self
     subclass: (spec at: 1) asAsymbol
     instanceVariableNames: (spec at: 2)
     classVariableNames: (spec at: 3 ifAbsent: [''])
     poolDictionaries: ''
     category: nil !
!
        
"===| Employee |=============================="
(Object kid: #('Employee' 'name age shoesize'))
  inits;
  public !


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
|x all|
x := (Employee new)  name: 'tammy'.
y := (Employee new)  name: 'tammy'.


x := OrderedCollection new.



