module main;
from imptypes import RT;
import imported;
type
    RT = pointer to record
	i, j : integer;
    end;
var
    p : imported.T;
    q : imported.OT;
    r : RT;
begin
    new(r);
    r^.i := 3;
    r^.j := 4;
    imported.Blah;
end main.
