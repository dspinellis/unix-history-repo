(*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)simple.p	8.1 (Berkeley) %G%
 *)

program pdxtest( output );
var
    x : real;
begin
    x := 0.5;
    x := 0.0;
    writeln( '17.0/x ', 17.0/x );
end.
