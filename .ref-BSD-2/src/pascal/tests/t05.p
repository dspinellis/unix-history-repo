program egwhile(output);
var
	n: integer;
	h: real;
begin
	n := 10;
	write(n);
	h := 0;
	while n > 0 do
	begin
		h := h + 1/n;
		n := n-1;
	end;
	writeln(h)
end.
