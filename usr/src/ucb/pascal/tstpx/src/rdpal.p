program bench3(input, output);
    const max=100;
    var i,len: integer; inline: packed array[1..max] of char; palind: Boolean;
begin
    while not eof do begin
	len := 0;
	while not eoln do
	    begin len := len+1; inline[len] := input^; get(input) end;
	readln;
	while (len > 1) and (inline[len] = ' ') do
	    len := len - 1;
        palind := true;
	for i := len downto len div 2 do
	    if inline[i] <> inline[len-i+1] then
		palind := false;
	for i := 1 to len do write(inline[i]);
	write(' is'); if not palind then write(' not');
	writeln(' a palindrome');
    end
end.
