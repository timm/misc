
@with_kw mutable struct It
  char = (skip='?',less='<',more='>',num='$',klass='!')
  str  = (skip="?")
  some = (max=32,bins=.5, cohen=0.3, trivial=1.05)
  seed = 1
end

````

