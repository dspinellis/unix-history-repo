/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)resume.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * Resume execution, first setting appropriate registers.
 */

#include "defs.h"
#include <signal.h>
#include "process.h"
#include "machine.h"
#include "main.h"
#include "process.rep"
#include "runtime/frame.rep"

#include "machine/pxerrors.h"
#include "pxinfo.h"

/*
 * Resume execution, set (get) pcode location counter before (after) resuming.
 */

resume()
{
    register PROCESS *p;

    p = process;
    do {
	if (option('e')) {
	    printf("execution resumes at pc 0x%x, lc %d\n", process->pc, pc);
	    fflush(stdout);
	}
	pcont(p);
	dread(&pc, PCADDR, sizeof(pc));		/* Get pcode pc */
	if (option('e')) {
	    printf("execution stops at pc 0x%x, lc %d on sig %d\n",
		process->pc, pc, p->signo);
	    fflush(stdout);
	}
    } while (p->signo == SIGCONT);
    if (option('r') && p->signo != 0) {
	choose();
    }

    /*
     * If px implements a breakpoint by executing a halt instruction
     * the real pc must be incremented to skip over it.
     *
     * Currently, px sends itself a signal so no incrementing is needed.
     *
	if (isbperr()) {
	    p->pc++;
	}
     */
}

/*
 * Under the -r option, we offer the opportunity to just get
 * the px traceback and not actually enter the debugger.
 *
 * If the standard input is not a tty but standard error is,
 * change standard input to be /dev/tty.
 */

LOCAL choose()
{
    register int c;

    if (!isterm(stdin)) {
	if (!isterm(stderr) || freopen("/dev/tty", "r", stdin) == NIL) {
	    unsetsigtraces(process);
	    pcont(process);
	    quit(process->exitval);
	    /* NOTREACHED */
	}
    }
    fprintf(stderr, "\nProgram error");
    fprintf(stderr, "\nDo you wish to enter the debugger? ");
    c = getchar();
    if (c == 'n') {
	unsetsigtraces(process);
	pcont(process);
	quit(process->exitval);
    }
    while (c != '\n' && c != EOF) {
	c = getchar();
    }
    fprintf(stderr, "\nEntering debugger ...");
    init();
    option('r') = FALSE;
    fprintf(stderr, " type 'help' for help.\n");
}
