program pointer(input, output);
type	xp = ^x;
	x = record
		y : integer;
		a : real;
	end;

var	p : xp;
begin
	new(p);
	p^.y := 5;
	p^.a := 3.14;
	writeln('pointer test');
end.
