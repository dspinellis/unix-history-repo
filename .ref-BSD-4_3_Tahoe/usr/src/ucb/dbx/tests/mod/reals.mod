(*
 * Test of reals and longreals.
 *)

module main;
var
    x : longreal;
    y : real;

procedure f (x : real) : longreal;
begin
    return longfloat(3.14*x);
end f;

begin
    y := 3.0;
    x := f(y);
end main.
