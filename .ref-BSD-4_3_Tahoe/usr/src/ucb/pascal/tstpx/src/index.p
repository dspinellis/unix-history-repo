program t(output);
var
a :array[1..10, 1..10] of integer;
i :1..10;
j, count :integer;
procedure foo;
var
a :array[1..10, 1..10] of integer;
i :1..10;
procedure bar;
begin
a[1,i] := 210;
a[2,i] := 220;
a[i,1] := 230;
a[i,2] := 240;
	if j = count then
		writeln('lvl2:', a[1,5], a[2,5], a[5,1], a[5,2]);
end;
begin
i := 4;
a[1,i] := 110;
a[2,i] := 120;
a[i,1] := 130;
a[i,2] := 140;
	if j = count then
		writeln('lvl1:', a[1,4], a[2,4], a[4,1], a[4,2]);
i := 5;
bar;
end;
begin
writeln('enter repeat count');
readln(count);
for j := 1 to count do begin
	i := 3;
	a[1,i] := 10;
	a[2,i] := 20;
	a[i,1] := 30;
	a[i,2] := 40;
	if j = count then
		writeln('main:', a[1,3], a[2,3], a[3,1], a[3,2]);
	foo;
end;
end.
