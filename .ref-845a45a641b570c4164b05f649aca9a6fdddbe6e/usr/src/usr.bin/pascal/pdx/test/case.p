(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)case.p	5.1 (Berkeley) %G%
 *)

program casetest(input, output);
var c : char;
	s : 0..1000;
	i : integer;
begin
	c := 'a';
	case c of
		'b': writeln('b');
		'c': writeln('c');
		'a': writeln('a');
	end;
	s := 3;
	case s of
		5: writeln('5');
		3: writeln('3');
		7: writeln('7');
	end;
	i := 1001;
	case i of
		0: writeln('0');
		-1: writeln('-1');
		1001: writeln('1001');
		-1001: writeln('-1001');
	end;
end.
