program graph2(output);
const
	d = 0.0625;
	s = 32;
	h1 = 34;
	h2 = 68;
	c = 6.28318;
	lim = 32;
var
	i,j,k,n: integer;
	x,y: real;
	a: array[1..h2] of char;
begin
	for j := 1 to h2 do
		a[j] := ' ';
	for i := 0 to lim do
	begin
		x := d*i;
		y := exp(-x)*sin(c*x);
		a[h1] := ':';
		n := round(s*y) + h1;
		a[n] := '*';
		if n < h1 then
			k := h1 else
			k := n;
		for j := 1 to k do
			write(a[j]);
		writeln;
		a[n] := ' ';
	end
end.
