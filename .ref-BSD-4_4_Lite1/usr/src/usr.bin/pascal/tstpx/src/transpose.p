program bench2(output);
    const size = 10;
    var i, j: integer; a: array[1..size,1..size] of char; ch: char;

procedure print;
begin
    for i := 1 to size do begin
	for j := 1 to size do
	    write(a[i,j]);
	writeln
    end
end;

begin
    for i := 1 to size do
	for j := 1 to size do
	     a[i,j] := chr(ord('a')+2*i+j-2);
    print;
    for i := 1 to size do
	for j := i+1 to size do
	     begin ch := a[i,j]; a[i,j] := a[j,i]; a[j,i] := ch end;
    writeln; print
end.
