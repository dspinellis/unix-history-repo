program sideffect(output);
var
	a, z: integer;
function sneaky(x: integer): integer;
	begin
		z := z-x;
		sneaky := sqr(x);
	end;
begin
	z := 10;
	a := sneaky(z);
	writeln(a, z);
	z := 10;
	a := sneaky(10) * sneaky(z);
	writeln(a, z);
	z := 10;
	a := sneaky(z) * sneaky(10);
	writeln(a, z);
end.
