{TEST 6.6.3.1-5, CLASS=CONFORMANCE}

{ When a procedure (or function) with a parameter list is
  included in the formal parameter list of another procedure
  (or function), the identifiers in the parameter list of the
  procedure parameter have defining occurences for that list
  and the corresponding block for the procedure only, and not
  for the block of the procedure to which it is passed.
  The example in this program should be passed by the compiler. }

program t6p6p3p1d5(output);
var
   i : integer;
procedure alsoconforms(x : integer);
begin
   writeln(' PASS...6.6.3.1-5')
end;

procedure conforms(procedure alsoconforms(x : integer));
   var x : boolean;
begin
   x:=true;
   alsoconforms(1)
end;

begin
   i:=2;
   conforms(alsoconforms)
end.
