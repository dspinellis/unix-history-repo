program recursivegcd(output);
var
	x, y, n: integer;
function gcd(m,n: integer): integer;
	begin
		if n=0 then
			gcd := m else
			gcd := gcd(n, m mod n);
	end;
procedure try(a, b: integer);
	begin
		writeln(a, b, gcd(a, b));
	end;
begin
	try(18, 27);
	try(312, 2142);
	try(61, 53);
	try(98, 868);
end.
