(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)recur.p	5.1 (Berkeley) %G%
 *)

program recursion(input, output);
var	i : integer;

function fact(n : integer) : integer;
begin
	if n <= 1 then begin
		fact := 1;
	end else begin
		fact := n * fact(n-1);
	end;
end;

begin
	i := 3;
	writeln(i:1, '! = ', fact(i):1);
end.
