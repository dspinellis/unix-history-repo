program t( output , input );
    begin
	while not eof do begin
	    while not eoln do begin
		if input^ in [ '+' , '-' , '*' , '/' , '%' ] then
		    case input ^ of
			'+':	writeln( 'plus' );
			'-':	writeln( 'minus' );
			'*':	writeln( 'star' );
			'/':	writeln( 'slash' );
			'%':	writeln( 'percent' );
		    end
		else
		    writeln( 'random char' );
		get(input);
	    end;
	    get(input);
	end;
    end.
