program test (input, output);
type
    Color = (RED, BLUE, GREEN);
    IntArray = array [1..10] of integer;
var a : IntArray;
    i : integer;
    b : array [Color] of integer;
    c : Color;

procedure p (i : integer; var a : IntArray; j : integer);
begin
    a[3] := i;
    a[4] := j;
end;

begin
    for i := 1 to 10 do begin
	a[i] := i;
    end;
    p(4, a, 5);
    b[BLUE] := 3;
    c := RED;
end.
