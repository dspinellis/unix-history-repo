{$t- No runtime tests}

program cfor(input,output);

type
chrint=-120..120;
sint=-1000..1000;
base=array[1..2] of record
	i :chrint;
	j :sint;
	k :integer;
	l :real;
	end;

var
i :chrint;
j :sint;
k :integer;
rec :base;

begin
writeln('Enter -> 10, 1000, 100000, 1.0e+10');
with rec[1] do begin
	read(i,j,k,l);
	writeln('Data echo ->',i,j,k,l);
	end;
for i:=-1 to 1 do
	case i of
	0: write(' loop');
	1: writeln(' works');
       -1: write('This');
	end;
for j:=500 to 501 do
	case j of
	500: write('Case');
	501: writeln(' checks');
	end;
for k:=54000 to 54002 do
	case k of
	54000: write('Success');
	54002: writeln(' assurred');
	54001:write(' is');
	end;
write('i = ');
for i:=5 downto 3 do
	write(i);
writeln;
write('j = ');
for j:=1000 downto 998 do
	write(j);
writeln;
write('k = ');
for k:=100001 downto 99999 do
	write(k);
writeln;
writeln('case and for work':20,'!':2,'!');
write('Enter a character here ->',input^,'<- there it is');
end.
