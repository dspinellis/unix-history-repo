program forfunc( output );
    
    var
	x, z : integer;

    function foo : integer;
	begin
	    z := z + 1;
	    foo := z;
	end;

    function bar( function y : integer ; x : integer ) : integer;
	begin
	    bar := y;
	    writeln('in bar x = ', x:1);
	end;

    begin
	z := 1;
	x := bar (foo, foo);
        writeln('in forfunc bar = ', x:1);
    end.
