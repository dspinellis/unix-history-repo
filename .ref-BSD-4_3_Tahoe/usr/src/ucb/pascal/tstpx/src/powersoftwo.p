program powersoftwo(output);
const m = 30; n = 90;  { m >= n*log(2) }
var exp,i,j,l: integer;
    c,r,t: integer;
    d: array [0..m] of integer;  {positive powers}
    f: array [1..n] of integer;  {negative powers}
begin l := 0; r := 1; d[0] := 1;
  writeln(wallclock);
  for exp := 1 to n do
  begin {compute and print 2**exp }  c := 0;
    for i := 0 to l do
    begin t := 2*d[i] + c;
      if t >= 10 then
        begin d[i] := t-10; c := 1;
        end
      else
        begin d[i] := t; c:= 0;
        end
    end ;
    if c > 0 then
      begin l := l+1; d[l] := 1
      end ;
    for i := m downto l do write(' ');
    for i := l downto 0 do write(d[i]:1);
    write(exp:5, '  .');
    {compute and print 2**(-exp) }
    for j := 1 to exp-1 do
    begin r := 10*r + f[j];
      f[j] := r div 2; r := r - 2*f[j]; write(f[j]:1)
    end ;
    f[exp] := 5; writeln('5'); r := 0
  end ;
  writeln(wallclock)
end .
