program enum(input, output);
const	BLAH = 3;
type	Color = (RED, GREEN, BLUE);
	String = array[1..5] of char;
var	i : (red, green, blue);
	s : array[Color] of String;
	c : char;
begin
	i := blue;
	c := 'a';
	s[RED] := 'red';
	writeln('test of enumerated types');
end.
