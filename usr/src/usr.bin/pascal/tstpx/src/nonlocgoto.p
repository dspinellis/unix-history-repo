program test(output);
label	1;

procedure lvl1;

var
f :file of char;

procedure lvl2;

begin
writeln('lvl2 called');
goto 1;
halt;
end;

begin
rewrite(f,'file');
writeln(f,'lvl1 called');
lvl2;
halt;
end;

begin
writeln('lvl0 called');
lvl1;
halt;
1:writeln('it worked');
end.
