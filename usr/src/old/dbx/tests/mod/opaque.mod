(*
 * Test of dealing with an opaque type that is never defined.
 *)

module main;

from imptypes import RT;
from io import Writef, output;

type
    T = record
	a : integer;
	b : RT;
	c : integer;
    end;

var r : T;

begin
    r.a := 3;
    r.c := 4;
    Writef(output, "this is a test\n");
end main.
