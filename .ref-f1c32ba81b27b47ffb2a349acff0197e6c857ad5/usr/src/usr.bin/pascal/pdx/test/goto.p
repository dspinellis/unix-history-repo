(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)goto.p	5.1 (Berkeley) %G%
 *)


program gototest(input, output);
label 1;

procedure A;
begin
	writeln('A');
	goto 1;
end;

procedure B;
begin
	writeln('B');
	A;
end;

begin
	B;
1:
	writeln('exiting');
end.
