program binrdwt(input, output);

const
LAST=1;

type
this=array[1..1024] of char;

var
binfil :file of this;
i :integer;
x :this;

begin
rewrite(binfil);
for i := 1 to LAST do
	write(binfil, x);
reset(binfil);
for i := 1 to LAST do
	read(binfil, x);
end.
