program test(output);

var
ch :char;
f :file of char;

begin
rewrite(f);
writeln(f,'abc');
write(f,'	');
reset(f);
repeat
  read(f,ch);
  writeln('in main: eoln = ',eoln(f),'   eof = ',eof(f),'   ch = ',ch);
  if eoln(f) and not eof(f) then begin
    read(f,ch);
    writeln('in eolncode: eoln = ',eoln(f),'   eof = ',eof(f),'   ch = ',ch);
    end
  until eof(f)
end.
