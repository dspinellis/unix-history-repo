program bench1(output);
    const size = 300;
    var i,j,t: integer; a: array[1..size] of integer;
begin for i := 1 to size do a[i] := size + 1 - i;
    for i := size-1 downto 1 do
	for j := i to size-1 do
	    if a[j] > a[j+1] then
		begin t := a[j]; a[j] := a[j+1]; a[j+1] := t end;
    { for i := 1 to size do writeln(a[i]) }
end.
