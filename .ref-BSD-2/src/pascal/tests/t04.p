program roman(output);
var
	x,y: integer;
begin
	y := 1;
	repeat
		x := y;
		write(y, ' ');
		while x >= 1000 do
		begin
			write('m');
			x := x-1000
		end;
		while x >= 500 do
		begin
			write('d');
			x := x-500
		end;
		while x >= 100 do
		begin
			write('c');
			x := x-100
		end;
		if x >= 50 then
		begin
			write('l');
			x := x-50
		end;
		while x >= 10 do
		begin
			write('x');
			x := x-10
		end;
		if x >= 5 then
		begin
			write('v');
			x := x-5
		end;
		while x >= 1 do
		begin
			write('i');
			x := x-1
		end;
		writeln;
		y := 2*y;
	until y > 5000
end.
