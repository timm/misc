#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "my"
@include "lib"
@include "num"
@include "sym"
@include "shared"

function DivX(i,rows,r) { return rows[r].cells[i.x] }
function DivY(i,rows,r) { return rows[r].cells[i.y] }

function Div(i,rows,xpos,ypos,yis) {
  is(i,"Div")
  i.cohen    = 0.2
  i.step     = 0.5
  i.maxDepth = 15
  i.x        = xpos
  i.y        = ypos
  i.yis      = yis
  has(i,"cuts")
  has(i,"xall","Num")
  y = DivY(i,rows,1)
  has(i,"yall", typeof(y)=="number" ? "Num" : "Sym")
  for(r in rows) {
    x = DivX(i,rows,r)
    y = DivY(i,rows,r)
    if (x ~ MY.skip) next
    i.start = i.start ? i.start : x  
    i.stop  = x
    Add(i.xall, x)
    Add(i.yall, y) 
  } 
  i.step  = length(rows)^i.step
  i.epsilon = Var(i.xall)*i.cohen
}
  
function DivCut(i,rows,lo,hi,lvl,xr,yr,depth,
            yall1) {
  
  copy(yr, yall)
  yis = i.yes
  if (depth < i.maxDepth)  
    if (hi - lo > i.step) {
      min = Var(yr)
      Num(xl)
      &yis(yl)
      for(r=lo; r<=hi; r++) 
      { x = DivX(i,rows,r)
        y = DivY(i,rows,r)
        Add(xl,x); Dec(xr, x)
        Add(yl,y); Dec(yr, y)
        if (x ~ MY.skip) next;
        x1 = DivX(i,rows,r+1)
        if ((r <= hi-i.step)           &&
            (x != x1)                  &&
            (x1 - i.start > i.epsilon) &&
            (i.stop - x   > i.epsilon) &&
            (Mid(xr) - Mid(xl) > i.epsilon)) 
        { new = Xpect(yl,yr) 
          if (new*i.trivial < min) 
          { min = new
            cut = j
            copy(xr, xr1); copy(xl, xl1)
            copy(yr, yrl); copy(yl, yl1) }}}
   }

            
        
 
           
}
