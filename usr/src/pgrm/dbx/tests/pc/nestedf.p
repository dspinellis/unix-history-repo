(*
 * Test of nested functions.
 *)

program nested (input, output);
var
    i, k : integer;

function f (var i : integer) : integer;
var
    j, k : integer;

    function nestedf (var j : integer) : integer;
    var
	i : integer;
    begin
	i := j + 2;
	j := i;
	nestedf := j;
    end;

begin
    j := i + 1;
    i := nestedf(j);
    i := j;
    f := i;
end;

begin
    i := 3;
    i := f(i);
end.
