program argtest(input, output);
var i : integer;
	s : array[1..10] of char;
begin
	for i := 1 to argc - 1 do begin
		argv(i, s);
		writeln('arg ', i:1, ' = ', s);
	end;
	write('i? ');
	readln(i);
end.
