program palindromes(output);
  var i,j,l,n,r,s: integer;
      p: boolean;
      d: array [1..10] of integer;
begin n := 0; writeln(wallclock);
  repeat n := n+1; s := n*n; l := 0;
    repeat l := l+1; r := s div 10;
      d[l] := s - 10*r; s := r
    until s = 0;
    i := 1; j := l;
    repeat p := d[i]=d[j];
      i := i+1; j := j-1
    until (i>=j) or not p;
    if p then writeln(n,n*n)
  until n = 200;
  writeln(wallclock)
end .
