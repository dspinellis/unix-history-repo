program quicksort(output);
  const n = 10000;
  var i,z: integer;
      a: array[1..n] of integer;

  procedure sort(l,r: integer);
    var i,j,x,w: integer;
  begin {quicksort with recursion on both partitions}
    i := l; j := r; x := a[(i+j) div 2];
    repeat
      while a[i] < x do i := i+1;
      while x < a[j] do j := j-1;
      if i <= j then
        begin w := a[i]; a[i] := a[j]; a[j] := w;
          i := i+1; j := j-1
        end
    until i > j;
    if l < j then sort(l,j);
    if l < r then sort(i,r);
  end { sort } ;

begin z := 1729;  {generate random sequence}
  for i := 1 to n do
    begin z := (131071*z) mod 2147483647; a[i] := z
    end ;
  writeln(clock);
  sort(1,n);
  writeln(clock);
  for i := 1 to n-1 do
   if a[i] > a[i+1] then writeln(i,a[i],a[i+1]);
end .
