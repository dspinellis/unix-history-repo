program ancestor2(output);
{ancestor algorithm using sets instead of boolean matrix}
  const n = 20;
  var i,j: integer;
      r: array [1..n] of set of 1..n;
begin { j in r[i] = "i is a parent of j"}
  for i := 1 to n do
    if i mod 10 <> 0 then r[i] := [i+1] else r[i] := [];
  writeln(wallclock);
  for i := 1 to n do
    for j := 1 to n do
      if i in r[j] then
		r[j] := r[i]+r[j];
  writeln(wallclock);
  for i := 1 to n do
  begin write(' ');
    for j := 1 to n do
      if j in r[i] then write('1') else write('.');
    writeln
  end ;
  writeln(wallclock)
end .
