program expon2(output);
var
	pi, spi: real;

function power(x: real; y: integer): real;
	var
		z: real;
	begin
		z := 1;
		while y>0 do
		begin
			while not odd(y) do
			begin
				y := y div 2;
				x := sqr(x);
			end;
			y := y-1;
			z := x*z;
		end;
		power := z;
	end;
begin
	pi := 3.14159;
	writeln(2.0, 7, power(2.0, 7));
	spi := power(pi, 2);
	writeln(pi, 2, spi);
	writeln(spi, 2, power(spi, 2));
	writeln(pi, 4, power(pi, 4));
end.
