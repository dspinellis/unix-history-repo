program ffunc( output );
    type
	comparisons = ( less , equal , greater );
    function insert(
	    function comparitor( this , that : integer ) : comparisons;
	    here , there : integer ) : boolean;
	function equality : boolean;
	    begin
		if comparitor( here , there ) = equal then begin
		    equality := true
		end else begin
		    equality := false
		end
	    end;
	begin
	    insert := equality;
	end;
    function compare( this , that : integer ) : comparisons;
	begin
		 if this < that then compare := less
	    else if this = that then compare := equal
	    else compare := greater
	end;
    begin
	writeln( insert( compare , 17 , 23 ) );
    end.
