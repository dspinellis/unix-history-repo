program perm(input,output);
const n=4; nfact=24; {make a table of permutations of 1..4}
type row=array[1..n] of 1..n;
var table:array[1..nfact] of row; i,j:integer;

{Generation of permutations in lexicographic order,
adapted from CACM Algorithm 202 (Mok-Kong Shen)}
procedure perle (var s:row); {s is a row consisting of the nth permutation,
			     and will be changed to contain the n+1 st}
label 1;
var j,u,w:integer;
begin
	w:=n; {permuting integers 1..n}
	while s[w]<s[w-1] do w:=w-1;
	u:=s[w-1];
	for j:= n downto w do
	begin
		if s[j]>u then begin s[w-1]:=s[j];
				     s[j]:=u;
				     goto 1
			       end
	end;
1: 	for j:=0 to round((n-w-1)/2 +0.1) do
	  begin u:= s[n-j];
		s[n-j]:=s[w+j];
		s[w+j]:= u
	  end
end; {of perle}
begin {main program}
for i:=1 to n do table[1][i]:=i; {initialize first row}
for i:=2 to nfact do begin
	table[i]:=table[i-1] {copy row};
	perle(table[i])
	end;
for i:=1 to nfact do begin for j:=1 to n do write(table[i][j]); writeln end;
end.
	

