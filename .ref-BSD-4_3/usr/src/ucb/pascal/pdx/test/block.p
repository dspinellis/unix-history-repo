program blocktest(input, output);
procedure A;
	procedure B;
	begin
		writeln('in procedure B');
	end;
begin
	writeln('in procedure A');
	B;
end;

begin
	writeln('in main program');
	A;
end.
