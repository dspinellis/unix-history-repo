module main;
type
    Rec = record
	charValue : char;
	intValue : integer;
	subrange : [0..1000];
	realValue : real;
    end;
var
    r : Rec;
begin
    r.charValue := 'c';
    r.intValue := 3;
    r.subrange := 10;
    r.realValue := 3.4;
end main.
