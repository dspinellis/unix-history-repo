module main;
var
    i : integer;

procedure p2 (i : integer);
begin
    if i < 5 then
	p2(i+1);
    end;
end p2;

procedure p1 (i : integer);
begin
    p2(i+1);
end p1;

begin
    i := 0;
    p1(i+1);
end main.
