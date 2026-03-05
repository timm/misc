"--- Class Definitions (Modern Fluid Syntax) ---"
Object << #Magic
    package: 'Generated-Scripts'.
(Smalltalk at: #Magic) install.

Magic << #Employee
    slots: #(name age shoesize);
    package: 'Generated-Scripts'.
(Smalltalk at: #Employee) install.

"--- Method Extensions ---"

!Object methods!
s
  ^self printString!

oo
   Transcript show: self s; cr!

visit: aBlock
  ^aBlock value: self!!

!Symbol methods!
talk
  ^self asString talk!!

!String methods!
talk
  self asFileReference readStreamDo: [ :s | 
      CodeImporter evaluateStream: s ]!

asWords
  ^self findTokens: Character separators!

left: n with: char
  | out |
  out := WriteStream on: (String new: n).
  out nextPutAll: self.
  1 to: n - self size do: [:i | out nextPut: char ].
  ^out contents!!

!Collection methods!
visit: aBlock
  self do: [:x | x visit: aBlock]!

b4Now: block
  | b4 |
  self do: [:now |
    b4 ifNotNil: [block value: b4 value: now].
    b4 := now]!!

!Magic class methods!
new
  ^super new init!!

!Magic methods!
init
  ^self!

visit: aBlock
  1 to: self class instSize do: [:i | 
     (self instVarAt: i) visit: aBlock ]!!

!Employee methods!
init
  name := 'freda'.
  age := 21.
  shoesize := 0!

printOn: aStream
  aStream 
     nextPutAll: 'Emp(';
     nextPutAll: ':name ', name s;
     nextPutAll: ' :age ', age s;
     nextPutAll: ' :shoesize ', shoesize s;
     nextPutAll: ')' !!
