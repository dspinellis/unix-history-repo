module main;
type
    Color = (RED, BLUE, GREEN);
var
    a : array [1..10] of integer;
    i : integer;
    b : array Color of integer;
    c : Color;

procedure p (i : integer; var a : array of integer; j : integer);
begin
    a[3] := i;
    a[4] := j;
end p;

begin
    for i := 1 to 2 do
	a[i] := i;
    end;
    p(4, a, 5);
    b[BLUE] := 3;
    c := RED;
end main.
