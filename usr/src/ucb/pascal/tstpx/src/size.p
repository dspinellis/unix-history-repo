program size(input,output);

type
str = array[1..20] of char;
bigary = array[1..10000] of integer;
{huge = array[1..20000] of integer;}
color = (black, purple, violet, red, pink, green,
	 blue, orange, yellow, white, none);

var
nums :bigary;
{mnums :huge;}
paint :color;

function bar( value :integer) :str;
begin
write(value:1);
bar := ' is the value';
end;

function foo :str;
begin
foo := 'this is a test';
end;

begin
writeln('Enter an integer');
read(nums[5]);
writeln(bar(nums[5]));
writeln(foo);
writeln('enter a color');
read(paint);
writeln('The next color is ',succ(paint));
end.
