program eightqueens(output);
const doprint = 1;
var i : integer;
    a : array [ 1..8 ] of boolean;
    b : array [ 2..16] of boolean;
    c : array [-7..7 ] of boolean;
    x : array [ 1..8 ] of integer;
    safe : boolean;

   procedure print;
      var k: integer;
   begin
	if doprint = 1 then begin
		write(' ');
	      for k := 1 to 8 do write(x[k]:2);
	      writeln;
	      end
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
      i := 0;
      repeat i := i+1; safe := a[i] and b[i+j] and c[i-j];
         if safe then
         begin setqueen; x[j] := i;
            if j < 8 then trycol(j+1) else print;
            removequeen
         end
      until i = 8
end;

begin print; for i := 1 to 8 do a[i] := true;
      for i := 2 to 16 do b[i] := true;
      for i := -7 to 7 do c[i] := true;
   trycol(1);
end.
