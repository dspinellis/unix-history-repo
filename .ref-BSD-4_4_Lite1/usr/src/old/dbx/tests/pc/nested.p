(*
 * Test of nested procedures and modules.
 *)

program nested (input, output);
var
    i, k : integer;

procedure p (var i : integer);
var
    j, k : integer;

    procedure nestedp (var j : integer);
    var
	i : integer;
    begin
	i := j + 2;
	j := i;
    end;

begin
    j := i + 1;
    nestedp(j);
    i := j;
end;

begin
    i := 3;
    p(i);
end.
