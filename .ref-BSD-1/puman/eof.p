program number(input, output);
label 1;
var i: integer;
begin
while true do begin
write('number, please ?');
if eof then
goto 1;
read(i);
writeln('that was an ', i:2);
end;
1:
end.
