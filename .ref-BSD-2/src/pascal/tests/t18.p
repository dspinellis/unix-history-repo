program main(output);
type
	t = record
		a: real;
		b: integer;
	end;
var
	x: t;
begin
	x := t(1.0, 1);
	writeln(x.a, x.b);
end.
