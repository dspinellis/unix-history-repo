program egwhile(output);
var
	i,n: integer;
	h: real;
begin
	n := 10;
	write(n);
	h := 0;
	for i := n downto 1 do
		h := h + 1/i;
	writeln(h)
end.
