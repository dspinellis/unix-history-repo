(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pointer.p	5.1 (Berkeley) %G%
 *)

program pointer(input, output);
type	xp = ^x;
	x = record
		y : integer;
		a : real;
	end;

var	p : xp;
begin
	new(p);
	p^.y := 5;
	p^.a := 3.14;
	writeln('pointer test');
end.
