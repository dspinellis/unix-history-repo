program testio(output);

const
str = 'foobar';
ch = 'c';
c13 = 13;
o13 = 13b;
r13 = 13.0;

type
color = (red, green, blue, yellow, orange, violet, purple);

var
hue :color;
r :real;
i :integer;
strg :alfa;

begin
r := 0;
for i := 1 to 15 do begin
	writeln(i:2,r:i,r:i:i,-r:i,-r:i:i,i:i:i,' ',i:i);
	r := 2*r+0.1;
	end;
i := 1;   r := 0;
writeln(i:2,r:1,r:1:1,-r:1,-r:1:1,i:1:1,' ',i:1);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:2,r:2:2,-r:2,-r:2:2,i:2:2,' ',i:2);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:3,r:3:3,-r:3,-r:3:3,i:3:3,' ',i:3);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:4,r:4:4,-r:4,-r:4:4,i:4:4,' ',i:4);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:5,r:5:5,-r:5,-r:5:5,i:5:5,' ',i:5);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:6,r:6:6,-r:6,-r:6:6,i:6:6,' ',i:6);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:7,r:7:7,-r:7,-r:7:7,i:7:7,' ',i:7);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:8,r:8:8,-r:8,-r:8:8,i:8:8,' ',i:8);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:9,r:9:9,-r:9,-r:9:9,i:9:9,' ',i:9);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:10,r:10:10,-r:10,-r:10:10,i:10:10,' ',i:10);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:11,r:11:11,-r:11,-r:11:11,i:11:11,' ',i:11);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:12,r:12:12,-r:12,-r:12:12,i:12:12,' ',i:12);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:13,r:13:13,-r:13,-r:13:13,i:13:13,' ',i:13);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:14,r:14:14,-r:14,-r:14:14,i:14:14,' ',i:14);
i := i+1;   r := 2*r+0.1;
writeln(i:2,r:15,r:15:15,-r:15,-r:15:15,i:15:15,' ',i:15);
for hue := red to purple do
	writeln(hue);
i := 13;
writeln(str,ch:2,ch,' ',13,c13,o13,r13,i,i hex,i oct,ch oct);
strg:='1234567890';
for i:=5 to 15 do begin
	writeln('->',strg:i,'<-');
	end;
end.
