{$p+}
program initable(input,output);
type  counter=0..4;
      object=1..4;
      container=set of object;
      row=array[1..4]of 1..4;
var   sol:row; i,j,k:integer;
      table:array[1..24]of row;

    procedure select(bag:container; sample,drawn:counter);
    {Generation of permutations in lexicographic order,
    adapted from Grogono, page 134.}
    var  ball:object;
    begin
    if drawn<sample then
	for ball:=1 to 4 do
      	    if ball in bag then begin sol[drawn+1]:=ball;
	 	                select(bag-[ball],sample,drawn+1)
 		       	       	end;
    if drawn=sample then begin table[k]:=sol; k:=k+1 end
    end; {of select}
    
begin {of initable}
    k:=1;
    select([1..4],4,0);
    for i := 1 to k-1 do begin
	write('table[', i:1, '] = ');
	for j := 1 to 4 do
	    write(table[i][j]);
	writeln;
	end;
end.
