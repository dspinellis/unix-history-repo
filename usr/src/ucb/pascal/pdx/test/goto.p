
program gototest(input, output);
label 1;

procedure A;
begin
	writeln('A');
	goto 1;
end;

procedure B;
begin
	writeln('B');
	A;
end;

begin
	B;
1:
	writeln('exiting');
end.
