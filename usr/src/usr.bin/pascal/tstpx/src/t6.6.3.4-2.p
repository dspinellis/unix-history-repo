{TEST 6.6.3.4-2, CLASS=CONFORMANCE}

{ This program tests that the environment of procedure
  parameters is as stated in the Pascal Standard.
  The compiler fails if the program does not compile, or
  the program states that this is so. }

program t6p6p3p4d2(output);
var
   globalone, globaltwo : integer;
procedure p(procedure f(procedure a; procedure b);procedure g);
   var
      localtop : integer;
   procedure r;
      begin
         if globalone=1 then
         begin
            if (globaltwo<>2) or (localtop<>1) then
               writeln(' FAIL1...6.6.3.4-2')
         end
         else
            if globalone=2 then
            begin
               if (globaltwo<>2) or (localtop<>2) then
                  writeln(' FAIL2...6.6.3.4-2')
               else
                  writeln(' PASS...6.6.3.4-2')
            end
            else
               writeln(' FAIL3...6.6.3.4-2');
         globalone:=globalone+1;
      end;     { of r }
   begin    { of p }
      globaltwo:=globaltwo+1;
      localtop:=globaltwo;
      if globaltwo=1 then
         p(f,r)
      else
         f(g,r)
   end;     { of p }
procedure q(procedure f; procedure g);
   begin
      f;
      g
   end;
procedure dummy;
begin
   writeln(' FAIL4...6.6.3.4-2')
end;

begin
   globalone:=1;
   globaltwo:=0;
   p(q,dummy)
end.
