(*
 * Test of reals.
 *)

program realtest (input, output);
var
    x : real;
    y : real;

function f (x : real) : real;
begin
    f := 3.14*x;
end;

begin
    y := 3.0;
    x := f(y);
end.
