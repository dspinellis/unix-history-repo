program random(output);

const
size = 110;
tries = 10000;

type
density=array[0..size] of integer;

var
den :density;
count :integer;
loc :integer;

begin
for count:=0 to size do
	den[count] := 0;
for count:=1 to tries do begin
	loc := round(random(1.0) * (size - 10));
	den[loc] := den[loc] + 1;
	end;
for count:=0 to size div 10 - 1 do begin
	for loc:=10 * count to 10 * count + 9 do
		write(den[loc]:7);
	writeln;
	end;
end.
