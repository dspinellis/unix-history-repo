/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pstatus.c	5.2 (Berkeley) %G%";
#endif not lint
/*
 * process status routines
 */

#include "defs.h"
#include <signal.h>
#include "process.h"
#include "machine.h"
#include "breakpoint.h"
#include "source.h"
#include "object.h"
#include "process.rep"

/*
 * Print the status of the process.
 * This routine does not return.
 */

printstatus()
{
    if (process->signo == SIGINT) {
	isstopped = TRUE;
	printerror();
    }
    if (isbperr() && isstopped) {
	skimsource(srcfilename(pc));
	printf("stopped at ");
	printwhere(curline, cursource);
	putchar('\n');
	if (curline > 0) {
	    printlines(curline, curline);
	} else {
	    printinst(pc, pc);
	}
	erecover();
    } else {
	isstopped = FALSE;
	fixbps();
	fixintr();
	if (process->status == FINISHED) {
	    quit(0);
	} else {
	    printerror();
	}
    }
}


/*
 * Print out the "line N [in file F]" information that accompanies
 * messages in various places.
 */

printwhere(lineno, filename)
LINENO lineno;
char *filename;
{
    if (lineno > 0) {
	printf("line %d", lineno);
	if (nlhdr.nfiles > 1) {
	    printf(" in file %s", filename);
	}
    } else {
	    printf("location %d\n", pc);
    }
}

/*
 * Return TRUE if the process is finished.
 */

BOOLEAN isfinished(p)
PROCESS *p;
{
    return(p->status == FINISHED);
}
