/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)wheredump.c 1.1 %G%";

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
		printf(", line %d\n", line);
		prevpc = frp->save_pc;
	}
	line = srcline(prevpc);
	printf("%s, line %d\n", name(program), line);
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
		prevpc = frp->save_pc;
		printf("%s", name(f));
		printparams(f, frp);
		printf(", line %d\n", line);
		dumpvars(f, frp);
		putchar('\n');
	}
	line = srcline(prevpc);
	printf("%s, line %d\n", name(program), line);
	dumpvars(program, NIL);
}
