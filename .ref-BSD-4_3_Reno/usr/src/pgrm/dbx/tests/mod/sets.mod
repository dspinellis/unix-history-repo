module sets;
type
    Color = (RED, BLUE, GREEN);
    ColorSet = set of Color;
var
    s : ColorSet;

procedure p (var s : ColorSet);
begin
    s := ColorSet{RED, BLUE};
end p;

begin
    p(s);
    if BLUE in s then
	s := s - ColorSet{BLUE};
    end;
end sets.
