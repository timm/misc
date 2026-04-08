program KnightsTour;
const N = 8; 
      DX: array[0..7] of integer = (2,1,-1,-2,-2,-1,1,2);
      DY: array[0..7] of integer = (1,2,2,1,-1,-2,-2,-1);
var B: array[1..N,1..N] of integer; 
    i,j,x,y,nx,ny,m,nxtX,nxtY,minD,d: integer;

function Deg(x,y: integer): integer; 
  var i,nx,ny,c: integer; begin
  c:=0;
  for i:=0 to 7 do begin 
    nx:=x+DX[i];
    ny:=y+DY[i];
    if (nx in [1..N]) and (ny in [1..N]) and (B[nx,ny]=0) then inc(c) end;
  Deg:=c end;

begin
  for i:=1 to N do for j:=1 to N do B[i,j]:=0;
  x:=1; y:=1; B[x,y]:=1;
  for m:=2 to N*N do begin
    minD:=9; nxtX:=-1;
    for i:=0 to 7 do begin
      nx:=x+DX[i];
      ny:=y+DY[i];
      if (nx in [1..N]) and (ny in [1..N]) and (B[nx,ny]=0) then begin
        d:=Deg(nx,ny);
       	if d < minD then begin minD:=d; nxtX:=nx; nxtY:=ny end end end;
    if nxtX=-1 then break; 
    x:=nxtX; y:=nxtY; B[x,y]:=m end;
  for j:=1 to N do begin 
    for i:=1 to N do write(B[i,j]:3); 
    writeln end end.
