program hugereal(input,output);
    var
	r : real;
	errno : integer;
    begin
	writeln('errno = ', errno);
	write('gimme a real ');
	readln(r);
	writeln('that was a really satisfying',r);
	writeln('errno = ', errno);
    end.
