# /* vim: set filetype=awk ts=2 sw=2 sts=2 expandtab: */

BEGIN { demo() }

function demo(     s,i) {
  Some(s)
  for(i=1;i<=100;i++) SomeAdd(s,i)
  print(110,s.n)
}

function Some(i,     most) {
  Object(i)
  i.most= def(most,64) # keep up to `most` number of items
  print(i.most)
  has(i,"all")             # i.all holds the kept value
  i.n=0
  i.sorted=0
}

function SomeAdd(i,v,    added,len) {
  i.n++
  i.sorted = 0
  len = length(i.all)
  if (len < i.most) {  # the cache is not full, add something
    push(i.all,v)
    added = 1
  } else if (rand() < len/i.n) {  # else, sometimes, add "v"
    i.all[ int(len*rand()) + 1 ] = v
    added = 1
  }
  return added
}
function SomeMedian(i,  m,n) {
  n= i.sorted ? length(i.all) : asort(i.all)
  i.sorted=1
  return median(i.all)
}

function csv(i,file,row,header,fs,rs,        
             fs0,rs0,txts,txt,cells, n,what) {
  header = header ? header : row
  fs0= FS
  rs0= RS
  FS = fs 
  RS = rs 
  print "FS[" FS "] RS[" RS "]"
  while((getline txt < file) > 0)  {
     gsub(/[ \t\r]*/, "", txt) # no whitespace:
     gsub(/#["*"]$/,     "", txt) # no comments
     if (txt) {
       txts = txts txt
       if (txts !~ /,$/) {
         print "\n["txts"]"
         print(">>", split(txts, cells, FS))
         txts = ""
         what = n ? row : header
	       @what(i,n,cells)
         n++ }}}
  close(file)
  FS= fs0
  RS= rs0
}
