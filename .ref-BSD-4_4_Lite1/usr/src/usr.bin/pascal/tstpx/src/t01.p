program graph1(output);
const
	d = 0.0625;
	s = 32;
	h = 34;
	c = 6.28318;
	lim = 32;
var
	x,y: real;
	i,n: integer;

begin
	for i := 1 to lim do
	begin
		x := x/i;
		y := exp(-x)*sin(c*x);
		n := round(s*y) + h;
		repeat
			write(' ');
			n := n-1;
		until n=0;
		writeln('*')
	end
end.
