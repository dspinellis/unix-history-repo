(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)call.p	5.1 (Berkeley) %G%
 *)

program calltest(input, output);
var i : integer;

procedure p(i : integer; r : real);
begin
    writeln('i = ', (i/0):1, ', r = ', r:1:2);
end;

procedure q(var i : integer);
begin
    i := 3;
end;

begin
    q(i);
    p(1, 3.4);
end.
