/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)pstatus.c 1.2 %G%";

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
		printf("stopped at ");
		if (curline > 0) {
			printf("line %d", curline);
			if (nlhdr.nfiles > 1) {
				printf(" in file %s", cursource);
			}
			putchar('\n');
			printlines(curline, curline);
		} else {
#			if (isvaxpx)
				printf("location %d\n", pc);
#			else
				printf("location 0x%x\n", pc);
#			endif
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
 * Return TRUE if the process is finished.
 */

BOOLEAN isfinished(p)
PROCESS *p;
{
	return(p->status == FINISHED);
}
