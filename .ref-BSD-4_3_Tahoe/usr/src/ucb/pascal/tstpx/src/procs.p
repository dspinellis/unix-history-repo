program procs(output,input,file3);

type
aray = array[1..20] of char;

var
a, d :alfa;
c :aray;
b :char;
i :integer;
file3 :file of char;

begin
writeln('Enter two lines');
while not eoln(input) do begin
	write(input^);
	get(input);
	end;
if input^ = ' ' then
	writeln;
readln(b);
if not eoln(input) then 
	halt
else
	writeln(b);
for i:=0 to argc-1 do begin
	argv(i,a);
	writeln('i = ',i:1,'   arg = ',a);
	end;
write('Jan ''70 plus ',wallclock:1,' seconds');
flush(output);
rewrite(file3);
write(file3,'test');
page(file3);
reset(file3);
linelimit(file3,20);
stlimit(5000);
date(a);   unpack(a,c,1);
time(a);   unpack(a,c,11);
pack(c,11,d);
pack(c,1,a);
writeln('Today is ',a,' at ',d);
assert(not undefined(1.0));
remove('file3');
message('This process has used ',sysclock:1,' + ',clock:1,' milliseconds');
i:=1;
end.
