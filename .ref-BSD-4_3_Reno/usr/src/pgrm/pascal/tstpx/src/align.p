program test(output);
type
name = array[1..4] of char;
rec = record
	x :boolean;
	y :name;
	end;
var
foo, bar :rec;
nm :name;
begin
foo.y := '    ';
foo.y[1] := 'n';
bar := foo;
nm := '    ';
nm[1] := 'n';
writeln('foo =', foo.y, '<- bar =', bar.y, '<- nm =', nm, '<-');
if (foo.y = nm) then
	writeln('cmp works')
else
	writeln('cmp fails');
end.
