(*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)file.p	8.1 (Berkeley) %G%
 *)

program filetest(input, output, testfile);
var testfile : text;
begin
    writeln('opening testfile');
    rewrite(testfile);
    writeln(testfile, 'all done');
end.
