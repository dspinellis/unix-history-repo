program Main(input, output);
procedure copy(var inp, out: text);
	var
		c: char;
	begin
		while not eof(inp) do
		begin
			while not eoln(inp) do
			begin
				c := inp^;
				out^ := c;
				put(out);
				get(inp);
			end;
			writeln;
			get(inp);
		end;
	end;
begin
	copy(input, output);
end.
