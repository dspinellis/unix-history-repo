/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)wheredump.c	5.1 (Berkeley) 6/6/85";
#endif not lint

/*
 * Print a list of currently active blocks starting with most recent.
 */

#include "defs.h"
#include "runtime.h"
#include "frame.rep"
#include "sym.h"
#include "machine.h"
#include "object.h"
#include "mappings.h"

where()
{
    FRAME *frp;
    ADDRESS prevpc;
    LINENO line;
    SYM *f;

    if (pc == 0) {
	error("program is not active");
    }
    prevpc = pc;
    for (frp = curframe(); frp != NIL; frp = nextframe(frp)) {
	f = whatblock(entry(frp));
	line = srcline(prevpc);
	printf("%s", name(f));
	printparams(f, frp);
	printf(", ");
	printwhere(line, srcfilename(prevpc));
	printf("\n");
	prevpc = frp->save_pc;
    }
    line = srcline(prevpc);
    printf("%s, ", name(program));
    printwhere(line, srcfilename(prevpc));
    printf("\n");
}

/*
 * Dump the world to the given file.
 * Like "where", but variables are dumped also.
 */

dump()
{
    FRAME *frp;
    ADDRESS prevpc;
    LINENO line;
    SYM *f;

    if (pc == 0) {
	error("program is not active");
    }
    prevpc = pc;
    for (frp = curframe(); frp != NIL; frp = nextframe(frp)) {
	f = whatblock(entry(frp));
	line = srcline(prevpc);
	printf("%s", name(f));
	printparams(f, frp);
	printf(", ");
	printwhere(line, srcfilename(prevpc));
	printf("\n");
	dumpvars(f, frp);
	putchar('\n');
	prevpc = frp->save_pc;
    }
    line = srcline(prevpc);
    printf("%s, ", name(program));
    printwhere(line, srcfilename(prevpc));
    printf("\n");
    dumpvars(program, NIL);
}
