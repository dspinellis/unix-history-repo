program Main(output);
type
	t = record
		a: real;
		b: integer;
	end;
var
	x: t;
begin
	with x do begin a:=1.0; b:=1; end;
	writeln(x.a, x.b);
end.
