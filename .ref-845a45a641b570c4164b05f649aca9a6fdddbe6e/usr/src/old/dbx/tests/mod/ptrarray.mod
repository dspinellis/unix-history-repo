module main;
var p : pointer to array [1..10] of integer;
    i : integer;
begin
    new(p);
    for i := 1 to 10 do
	p^[i] := i;
    end;
end main.
