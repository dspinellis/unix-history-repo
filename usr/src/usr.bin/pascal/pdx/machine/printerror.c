/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)printerror.c	5.1 (Berkeley) %G%";
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

printerror()
{
    register PROCESS *p;
    char *filename;
    int c;

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
	if (errnum != 0) {
	    printf(":  %s", pxerrmsg[errnum]);
	}
    } else {
	printf("\n\ninterrupt at ");
	printwhere(curline, cursource);
    }
    putchar('\n');
    printlines(curline, curline);
    erecover();
}
