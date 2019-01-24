FileStream fileIn: '0lib.st' !
FileStream fileIn: 'eg.st' !

Eg of: 'writing output'
do: [11 oo]!

Eg of: 'list of words' 
   do: [
  |w|
  w := 'a list of words' asWords . 
  w oo]
!

Eg of: 'oo' do:[ 1 oo]!

Eg cache!
Eg run!
  
