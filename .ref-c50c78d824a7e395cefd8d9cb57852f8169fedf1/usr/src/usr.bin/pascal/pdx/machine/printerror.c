/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)printerror.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * Print out an execution time error.
 */

#include "defs.h"
#include <signal.h>
#include "machine.h"
#include "sym.h"
#include "process.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "pxerrors.h"
#include "process/process.rep"

#ifdef tahoe
BOOLEAN shouldrestart;
#endif

printerror()
{
    register PROCESS *p;

    p = process;
    if (p->signo != ESIGNAL && p->signo != SIGINT) {
	error("signal %d at px pc %d, lc %d", p->signo, p->pc, pc);
    }
    curline = srcline(pc);
    curfunc = whatblock(pc);
    skimsource(srcfilename(pc));
    if (p->signo == ESIGNAL) {
	printf("\nerror at ");
	printwhere(curline, cursource);
        putchar('\n');
        printlines(curline, curline);
#ifdef tahoe
	/*
	 * this px is no good; it is easier to kill it and start
	 * a new one than to make it recover...
	 * make runtime/callproc.c know it too.
	 */
	shouldrestart = TRUE;
#endif
        erecover();
    } else {
	printf("\n\ninterrupt at ");
	printwhere(curline, cursource);
        putchar('\n');
        printlines(curline, curline);
        erecover();
    }
}
