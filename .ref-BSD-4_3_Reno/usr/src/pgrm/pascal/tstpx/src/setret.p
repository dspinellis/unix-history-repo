program setret(output);

type
bigset = set of char;

var
old :bigset;

function bigit(inp : bigset) :bigset;
begin
bigit := inp - ['a'..'z'];
end;

begin
old := ['g'..'w', 'A'..'S'];
writeln('before ', card(old));
writeln('after ',card(bigit(old)));
end.
