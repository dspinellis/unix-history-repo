program egwhile(output);
var
	n: integer;
	h: real;
begin
	n := 10;
	write(n);
	h := 0;
	repeat
		h := h + 1/n;
		n := n-1;
	until n = 0;
	writeln(h)
end.
