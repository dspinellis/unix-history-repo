program enumtest (input, output);
type
    Color = (RED, GREEN, BLUE);

var c1, c2 : Color;

procedure P (c : Color);
begin
    writeln('c = ', ord(c):1);
end;

begin
    c1 := BLUE;
    c2 := GREEN;
    P(RED);
end.
