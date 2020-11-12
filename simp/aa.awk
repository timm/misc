

function new(i) { split("",i,"") }

function Some(i) { new(i); 
function Some1(i,v,    m) {
    i.n++
    m = length(i.cache)
   if (m < i.most) {  # the cache is not full, add something
      i.sorted = 0
  13.      return push(i.cache,v)
  14.    }
  15.    if (rand() < m/i.n) {   # else, sometimes, add "v"
  16.      i.sorted = 0
  17.      return i.cache[ int(m*rand()) + 1 ] = v
  18.    }
  19.  }

