#Eg isa: Object with: 'name code' shares: 'cache' !

! Eg class methods !

cache
  "self initializing variable"
  cache == nil ifTrue: [
    cache := Dictionary new].
  ^cache
!
of: txt do: block
  "create, and cache, a new example."
  |x|
  x := self new.
  x name: txt; 
    code: block.
  self cache at: txt put: x
!
run
  "run all tests"
  self cache do: [:eg| eg run]. 
!!

! Eg methods !
run
  "run one method"
  Transcript cr;
     show: (5 of: $-),'| ',name,' |',(30 of: $-);
     cr.
  code value.
!!

