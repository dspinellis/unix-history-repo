(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fproc.p	5.1 (Berkeley) %G%
 *)

program fproc(output);
    var
    i :integer;

    procedure print(function frtn :integer);
	begin
	    write(frtn:3,'   formal routine =');
	end;

    procedure lvl1(function form: integer);
	label	1;
	var
	loc :integer;

	function eval :integer;
	    begin
		if loc = 8 then begin
			writeln(' non-local jump');
			goto 1;
		end;
		eval := loc;
	    end;

    begin
	loc := i;
	i := i - 1;
	if (loc = 4) or (loc = 8) then
		lvl1(eval)
	else if loc > 0 then
		lvl1(form);
1:	write('Stack frame:',loc:3,'   formal print =');
	print(form);
	writeln(form:3);
    end;

    function geval :integer;
	begin
	    geval := i;
	end;

    begin
	writeln('This should print levels 0-3, with formal values of 4.');
	writeln('Level 4 should jump to level 8.');
	writeln('Finally levels 8-10 should print with formal values of -1.');
	i := 10;
	lvl1(geval);
    end.
