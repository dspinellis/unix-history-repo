module main;

from io import Writef, output;

procedure r (n : integer);
begin
    Writef(output, "blah\n");
    if n > 0 then
	r(n - 1);
	Writef(output, "blah2\n");
    end;
end r;

begin
    r(5);
end main.
