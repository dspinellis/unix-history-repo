program sets (input, output);
type
    Color = (RED, BLUE, GREEN);
    ColorSet = set of Color;
var
    s : ColorSet;

procedure p (var s : ColorSet);
begin
    s := [RED, BLUE];
end;

begin
    p(s);
    if BLUE in s then begin
	s := s - [BLUE];
    end;
end.
