program countcharacters(input,output);
  var ch: char;
    c0,c1,c2,c3,c4: integer;  {counters}
begin writeln(clock); { linelimit(output, -1); }
  c0 := 0; c1 := 0; c2 := 0; c3 := 0; c4 := 0;
  while not eof(input) do
  begin write(' '); c0 := c0+1;
    while not eoln(input) do
    begin read(ch); write(ch);
      if ch = ' ' then c1 := c1+1 else
      if ch in ['a'..'z'] then c2 := c2+1 else
      if ch in ['0'..'9'] then c3 := c3+1 else c4 := c4+1
    end ;
    readln; writeln
  end ;
  writeln(clock);
  writeln(c0,' lines');
  writeln(c1,' blanks');
  writeln(c2,' letters');
  writeln(c3,' digits');
  writeln(c4,' special characters');
  writeln(clock)
end .
