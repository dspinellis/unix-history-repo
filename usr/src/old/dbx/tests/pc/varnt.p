program varnt(input, output);
type
	vrec = record
		rfield : integer;
		case integer of
		1: (
			vfield : integer;
		);
		2: (
			vfield2 : real;
		);
	end;

var
	r : vrec;
begin
	r.vfield := 1;
	r.vfield2 := 2.5;
	r.rfield := 0;
	writeln('done');
end.
