module main;
type
    Color = (RED, BLUE, GREEN);
var
    a : dynarray of array of integer;
    i, j : integer;

procedure p (i : integer; var a : array of array of integer; j : integer);
begin
    a[3, 1] := i;
    a[4, 2] := j;
end p;

begin
    new(a, 10, 5);
    for i := 0 to 9 do
	for j := 0 to 4 do
	    a^[i, j] := i;
	end;
    end;
    p(4, a^, 5);
end main.
