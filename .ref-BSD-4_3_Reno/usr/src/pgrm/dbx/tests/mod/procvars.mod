(*
 * Test of procedure variables.
 *)

module main;

procedure p (var i : integer);
begin
    i := 3;
end p;

procedure q ;
var
    t : procedure(var integer);
    j : integer;
begin
    t := p;
    t(j);
    j := j + 1;
end q;

begin
    q;
end main.
