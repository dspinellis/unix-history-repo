program strngs(output);

const
STRSZE = 10000;
CHKPT1 = 6800;
CHKPT2 = 3000;

type
longstr = array[1..STRSZE] of char;

var
str1, str2 :longstr;
i :integer;

begin
for i := 1 to STRSZE do begin
	str1[i] := chr((i mod 127) + 1);
	str2[i] := chr((i mod 127) + 1);
	end;
str1[CHKPT1] := 'a';
str2[CHKPT1] := 'b';
if str1 = str2 then
	writeln('= fails')
else
	writeln('= succeeds');
if str1 <> str2 then
	writeln('<> succeeds')
else
	writeln('<> fails');
if str1 > str2 then
	writeln('> fails')
else
	writeln('> succeeds');
if str1 < str2 then
	writeln('< succeeds')
else
	writeln('< fails');
if str1 >= str2 then
	writeln('>= fails')
else
	writeln('>= succeeds');
if str1 <= str2 then
	writeln('<= succeeds')
else
	writeln('<= fails');
str1[CHKPT2] := 'a';
str2[CHKPT2] := 'b';
if str1 = str2 then
	writeln('= fails')
else
	writeln('= succeeds');
if str1 <> str2 then
	writeln('<> succeeds')
else
	writeln('<> fails');
if str1 > str2 then
	writeln('> fails')
else
	writeln('> succeeds');
if str1 < str2 then
	writeln('< succeeds')
else
	writeln('< fails');
if str1 >= str2 then
	writeln('>= fails')
else
	writeln('>= succeeds');
if str1 <= str2 then
	writeln('<= succeeds')
else
	writeln('<= fails');
end.
