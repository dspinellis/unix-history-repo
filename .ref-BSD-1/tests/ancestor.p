program ancestor(output);
{R.W.Floyd: 'Ancestor', Comm.ACM 6-62 and 3-63, Alg.96}
  const n = 100;
  var i,j,k: integer;
      r: array [1..n, 1..n] of boolean;
begin { r[i,j] = "i is a parent of j"}
  for i := 1 to n do
    for j := 1 to n do r[i,j] := false;
  for i := 1 to n do
    if i mod 10 <> 0 then r[i,i+1] := true;
  writeln(clock);
  for i := 1 to n do
    for j := 1 to n do
      if r[j,i] then
        for k := 1 to n do
          if r[i,k] then r[j,k] := true;
  writeln(clock);
  for i := 1 to n do
  begin write(' ');
    for j := 1 to n do write(chr(ord(r[i,j])+ord('0')));
    writeln
  end ;
  writeln(clock)
end .
