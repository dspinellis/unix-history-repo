program frecur( output );
    var
	time : integer;
	direction : integer;
    function pad( j : integer ) : integer;
	const
	    indent = 4;
	begin
	    {}
	    if ( j <= 0 ) then begin
		j := 1;
	    end;
	    {}
	    pad := j * indent;
	end;
    procedure a( procedure xx );
	begin
	    writeln( ' ':pad(time) , 'a: about to	xx()' );
	    xx;
	    writeln( ' ':pad(time) , 'a: back from	xx()' );
	end;
    procedure b( procedure yy );
	procedure c;
	    begin
		writeln( ' ':pad(time) , 'c: about to	a(yy)' );
		time := time + direction;
		a( yy );
		writeln( ' ':pad(time) , 'c: back from	a(yy)' );
	    end;
	begin
	    if ( direction = 1 ) then begin
		if ( time > 10 ) then begin
		    direction := -direction;
		end;
		time := time + direction;
		writeln( ' ':pad(time) , 'b: about to	b(c)' );
		b( c );
		writeln( ' ':pad(time) , 'b: back from	b(c)' );
	    end else begin
		writeln( ' ':pad(time) , 'b: about to	yy' );
		yy;
		writeln( ' ':pad(time) , 'b: back from	yy' );
	    end;
	end;
	procedure d;
	    begin
		writeln( 'd:' );
	    end;
    begin
	time := 1;
	direction := 1;
	writeln( 'program: about to	b( d )' );
	b( d );
	writeln( 'program: back from	b( d )' );
    end.
