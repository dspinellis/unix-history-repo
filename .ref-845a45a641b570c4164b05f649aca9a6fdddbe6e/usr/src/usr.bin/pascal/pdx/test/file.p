(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)file.p	5.1 (Berkeley) %G%
 *)

program filetest(input, output, testfile);
var testfile : text;
begin
    writeln('opening testfile');
    rewrite(testfile);
    writeln(testfile, 'all done');
end.
