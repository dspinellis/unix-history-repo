program main(output);
const
	size = 10;
var
	aset: set of 1..size;
	i, j: integer;
begin
	aset := [];
	for i := 1 to size do
	begin
		aset := [i]+aset;
		for j := 1 to size do
			if j in aset then
				writeln(i,j);
	end;
end.
