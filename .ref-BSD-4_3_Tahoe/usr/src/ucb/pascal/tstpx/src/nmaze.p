
program randpath(input,output);
const maxwidth=122;maxheight=52;{lpt page size}
    n=4;nfact=24;
type row=array[1..n]of 1..n;
  mtype=(border,path,wall);
var maze:array[-2..maxwidth,-2..maxheight] of mtype;
  table:array[1..24]of row;
 xdir,ydir:array[1..4] of -2..2;
 width:1 .. maxwidth; height: 1..maxheight; startx,starty:integer;
  touchl,touchr:boolean {touched left and right edges};
  wannamaze:char;
{Generation of permutations in lexicographic order,
adapted from CACM Algorithm 202 (Mok-Kong Shen)}
procedure perle (var s:row); {s is a row consisting of the nth permutation,
			     and will be changed to contain the n+1 st}
label 1;
var j,u,w:integer;
begin
	w:=n; {permuting integers 1..n}
	while s[w]<s[w-1] do w:=w-1;
	u:=s[w-1];
	for j:= n downto w do
	begin
		if s[j]>u then begin s[w-1]:=s[j];
				     s[j]:=u;
				     goto 1
			       end
	end;
1: 	for j:=0 to round((n-w-1)/2 +0.1) do
	  begin u:= s[n-j];
		s[n-j]:=s[w+j];
		s[w+j]:= u
	  end
end; {of perle}
procedure initable;
var i:integer;
begin 
for i:=1 to n do table[1][i]:=i; {initialize first row}
for i:=2 to nfact do begin
	table[i]:=table[i-1] {copy row};
	perle(table[i])
	end;
end;
procedure init; {initialize maze}
var pip, i,j:integer;
begin
write('width=');readln(width);write('height=');readln(height);
write('randomizing seed=');readln(pip);
pip:=seed(pip);
for i:=-1 to 2*width+1 do for j:=-1 to 2*height+1 do maze[i,j]:=wall;
for i:=-2 to 2*width+2 do begin maze[i,-2]:=border;
  maze[i,2*height+2]:=border end;
for j:= -2 to 2*height+2 do begin maze[-2,j]:=border;
   maze[2*width+2,j]:=border end;
end;
procedure growtree(x,y,px,py:integer);
var i,m,t:integer; choice:row;
procedure fillin;
	begin
	maze[(x+px)div 2,(y+py)div 2]:=path;
	maze[x,y]:=path
	end;

begin {growtree}
if  maze[x,y] = wall 
  then begin fillin;
	  m:=round(random(1.0)*23.0)+1;  {random number between 1 and 24}
	  for i:=1 to 4 do 
	begin
		choice:=table[m]; {determine row in table}
		t:=choice[i]; {choice is 1, 2, 3, 4 ;; e s w n}
		growtree(x+xdir[t],y+ydir[t],x,y);
	end {of for} 
end {of then};
if (x=-2) and (touchl=false) then begin touchl:=true {touched left border};
					fillin
				  end;
if (x=2*width+2) and (touchr=false) then begin touchr:=true;
					fillin end

{otherwise, just return}
end; {of growtree}

procedure printmaze;
var i,j:integer;
begin for j:= 2*height+1 downto -1 do begin
	write(' ');
	for i:=-1 to 2*width+1 do case maze[i,j] of
	path:write(' ');
	border,wall:write('X'); {for lineprinter}
 	end; writeln
	end end;
begin {main}
xdir[1]:=2;xdir[2]:=0;xdir[3]:=-2;xdir[4]:=0;
ydir[1]:=0;ydir[2]:=-2;ydir[3]:=0;ydir[4]:=2;
wannamaze:= 'y';
initable;
while wannamaze='y' do
begin
init; 
touchl:=false;touchr:=false;
startx:= 2*(round((1.5+random(1.0))*width) div 4);
starty:= 2*(round((1.5+random(1.0))*height)div 4);
{ start near but not at middle }
growtree(startx,starty,startx,starty);
printmaze;
write('want another? (y or n)') ; readln(wannamaze);
end {wannamaze}
end.

