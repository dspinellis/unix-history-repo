module main;
type
    ElementType = (CHAR, INT, REAL);
    VR = record
	case tag : ElementType of
	    CHAR:
		charValue : char;|
	    INT:
		intValue : integer;|
	    REAL:
		realValue : real;
	end;
    end;
var
    vr : VR;
begin
    vr.tag := CHAR;
    vr.charValue := 'c';
    vr.tag := INT;
    vr.intValue := 3;
    vr.tag := REAL;
    vr.realValue := 3.4;
end main.
