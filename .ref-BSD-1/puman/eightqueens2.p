program eightqueens(output);
label 13;
var i : integer;
    a : array [ 1..8 ] of boolean;
    b : array [ 2..16] of boolean;
    c : array [-7..7 ] of boolean;
    x : array [ 0..7 ] of integer;
    safe : boolean;

   procedure print;
      var k, l: integer;
   begin
      writeln;
      writeln('*** Solution to the Eight Queens Problem ***');
      writeln;
      for l := 1 to 8 do begin
	 write(tab, 9 - l : 1, '  ');
	 for k := 1 to 8 do begin
	    if x[l] = k then
		write('Q ')
	    else if odd (k + l) then
		write('* ')
	    else
		write('- ')
	 end;
	 writeln
      end;
      writeln;
      writeln(tab, '   q q q q k k k k');
      writeln(tab, '   r n b     b n r');
      writeln;
      goto 13
   end ;

procedure trycol(j : integer);
   var i : integer;

   procedure setqueen;
   begin a[i] := false; b[i+j] := false; c[i-j] := false;
   end ;

   procedure removequeen;
   begin a[i] := true; b[i+j] := true; c[i-j] := true;
   end ;

    begin
      repeat i := i+1; safe := a[i] and b[i+j] and c[i-j];
         if safe then
         begin setqueen; x[j] := i;
            if j < 8 then trycol(j+1) else print;
            removequeen
         end
      until i = 8
end;

begin
      for i := 1 to 8 do a[i] := true;
      for i := 2 to 16 do b[i] := true;
      for i := -7 to 7 do c[i] := true;
      trycol(1);
      writeln('No solutions!');
      halt;
13:
end.
