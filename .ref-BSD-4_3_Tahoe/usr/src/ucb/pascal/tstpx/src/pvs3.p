{TEST 6.6.5.2-2, CLASS=ERRORHANDLING}

{ This program causes an error to occur as eof(f) does
  not yield false prior to execution of a get on the file f.
  The error should be detected at compile-time or run-time. }

program t6p6p5p2d2(output);
var
   fyle : text;
begin
   rewrite(fyle);
   writeln(fyle,'ABC');
   reset(fyle);
   get(fyle);            { fyle^='A' }
   get(fyle);           { fyle^='B' }
   get(fyle);           { fyle^='C' }
   get(fyle);           { fyle^ undefined...eof is true }
   get(fyle);           { error since eof is true }
   writeln(' ERROR NOT DETECTED...6.6.5.2-2')
end.
