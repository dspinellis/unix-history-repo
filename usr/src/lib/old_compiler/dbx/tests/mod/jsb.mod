module main;

from io import Writef, output;

var global : integer;

procedure p (i : integer; s : array of char; j : integer);
begin
    Writef(output, "in p(%d, %s, %d)\n", i, s, j);
    global := 10;
end p;

begin
    global := 3;
    p(3, "blah", 4);
end main.
