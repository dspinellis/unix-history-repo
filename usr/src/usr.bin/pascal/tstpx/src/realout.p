program testio(input, output);

var
work :array[1..20] of char;
doesit :boolean;
i, j :integer;
r :real;

begin
work := 'This program works? ';
doesit := true;
r := 31.4159;
i := 10;
j := 25;
writeln('i = ',i,' ':3,'j = ':1,j:1);
writeln('r = ',r,'   2 * r = ',2*r);
write('also r = ',r:1,'   and r = ',r:10);
writeln('   finally r = ',r:7:3);
writeln('and r = ',r:j:i);
writeln(work,doesit);
writeln(work:i,doesit:i);
writeln(work:j,doesit:1);
j:=2;
writeln(work:5,doesit:j);
end.
