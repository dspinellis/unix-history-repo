(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)args.p	5.1 (Berkeley) %G%
 *)

program argtest(input, output);
var i : integer;
	s : array[1..10] of char;
begin
	for i := 1 to argc - 1 do begin
		argv(i, s);
		writeln('arg ', i:1, ' = ', s);
	end;
	write('i? ');
	readln(i);
end.
