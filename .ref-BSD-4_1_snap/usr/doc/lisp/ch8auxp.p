type 	pinteger = ^integer;
	realarray = array[0..10] of real;
	pintarray = array[0..10] of pinteger;
	listoffixnumscell = record  
				cdr  : ^listoffixnumscell;
				fixnum : pinteger;
			    end;
	plistcell = ^listcell;
	listcell = record
		      cdr : plistcell;
		      car : integer;
		   end;

function pfoo ( var a : integer ; 
		var b : realarray;
		var c : pintarray;
		var d : listoffixnumscell) : integer;
begin
   writeln(' a:',a, ' b[0]:', b[0], ' b[1]:', b[1]);
   writeln(' c (first):', c[0]^,' c (second):', c[1]^);
   writeln(' ( ', d.fixnum^, d.cdr^.fixnum^, ' ...) ');
   b[1] := 3.1415926;
   pfoo := 3
end ;

{ the function pmemq looks for the lisp pointer given as the first argument
  in the list pointed to by the second argument.
  Note that we declare " a : integer " instead of " var a : integer " since
  we are interested in the pointer value instead of what it points to (which
  could be any lisp object)
}
function pmemq( a : integer; list : plistcell) : plistcell;
begin
 while (list <> nil) and (list^.car <> a) do list := list^.cdr;
 pmemq := list;
end ;
