{
  test of floating point exception handling
}

program fpe(input, output);
var x, y : real;
begin
	x := 1;
	y := 0;
	writeln(x/y);
end.
