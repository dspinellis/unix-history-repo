{TEST 6.6.5.4-1, CLASS=CONFORMANCE}

{ This program tests that pack and unpack are
  implemented in this compiler as according to the
  Standard.
  The compiler fails if the program does not compile. }

program t6p6p5p4d1(output);
type
   colourtype = (red,pink,orange,yellow,green,blue);
var
   unone    : array[3..24] of char;
   pacone   : packed array[1..4] of char;
   untwo    : array[4..8] of colourtype;
   pactwo   : packed array[6..7] of colourtype;
   i        : integer;
   colour   : colourtype;
begin
   pacone:='ABCD';
   unpack(pacone,unone,5);
   colour:=red;
   for i:=4 to 8 do
   begin
      untwo[i]:=colour;
      colour:=succ(colour)
   end;
   pack(untwo,5,pactwo);
   writeln('unone[5] = ''', unone[5], ''' = ', ord(unone[5]));
   if unone[5]='A' then
      writeln(' PASS...6.6.5.4-1')
   else
      writeln(' FAIL...6.6.5.4-1')
end.
