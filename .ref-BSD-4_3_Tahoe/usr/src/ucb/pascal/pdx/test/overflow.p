program fpexceptions(input,output);
    type
	fperrorkind = ( fperrorfirst,
			overflow,underflow,divideby0,domain,
			fperrolast );
    var
	request : fperrorkind;
    procedure genoverflow;
	var
	    i : integer;
	    r : real;
	begin
	    r := 2.0;
	    for i := 1 to 1000 do begin
		r := r * r;
	    end;
	    writeln('this machine handles more than 2^1000');
	end;
    procedure genunderflow;
	var
	    i : integer;
	    r : real;
	begin
	    r := 0.5;
	    for i := 1 to 1000 do begin
		r := r * r;
	    end;
	    writeln('this machine handles more than 2^-1000');
	end;
    procedure gendivideby0;
	var
	    r : real;
	begin
	    r := 17.0;
	    r := r - r;		{should be 0.0}
	    r := 17.0 / r;
	    writeln('i wonder what r is?', r);
	end;
    procedure gendomain;
	var
	    r : real;
	begin
	    r := -17.0;
	    r := sqrt(r);
	    writeln('i wonder what r is?', r);
	end;
    begin
	write('which do you want (');
	for request := succ(fperrorfirst) to pred(fperrolast) do begin
		{this isn't standard pascal.}
	    write( ' ', request);
	end;
	write(' ): ');
	    {neither is this, but it sure is convenient.}
	readln(request);
	if request in [overflow,underflow,divideby0,domain] then begin
	    writeln('one ', request, ' coming right down');
	    case request of
		overflow:	genoverflow;
		underflow:	genunderflow;
		divideby0:	gendivideby0;
		domain:	gendomain;
	    end;
	end else begin
		{default:}
	    writeln('oh, never mind');
	end;
    end.
	    
