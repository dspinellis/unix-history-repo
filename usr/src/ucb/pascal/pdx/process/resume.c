/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)resume.c	5.2 (Berkeley) 4/7/87";
#endif not lint

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

#if defined(vax) || defined(tahoe)
    LOCAL ADDRESS fetchpc();
#endif
#ifdef vax
#define	PCREG	11	/* where px holds virtual pc (see interp.sed) */
#endif
#ifdef tahoe
#define	PCREG	12	/* where px holds virtual pc (see interp.sed) */
#endif

#ifdef sun
LOCAL ADDRESS *pcaddr;
#endif

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
#       ifdef sun
	    if (pcaddr == 0) {
		dread(&pcaddr, PCADDRP, sizeof(pcaddr));
	    }
	    dread(&pc, pcaddr, sizeof(pc));
#       else vax || tahoe
	    if (p->status == STOPPED) {
		if (isbperr()) {
		    pc = p->reg[PCREG];
		} else {
		    dread(&pcframe, PCADDRP, sizeof(pcframe));
#ifdef tahoe
		    pcframe += 14;
#else
		    pcframe++;
#endif
		    pc = fetchpc(pcframe);
		}
		pc -= (sizeof(char) + ENDOFF);
	    }
#       endif
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

#if defined(vax) || defined(tahoe)

/*
 * Find the location in the Pascal object where execution was suspended.
 *
 * We basically walk back through the frames looking for saved
 * register PCREG's.  Each time we find one, we remember it.  When we reach
 * the frame associated with the interpreter procedure, the most recently
 * saved register PCREG is the one we want.
 */

typedef struct {
#ifdef vax
    int fr_handler;
    unsigned int fr_psw : 16;   /* saved psw */
    unsigned int fr_mask : 12;  /* register save mask */
    unsigned int fr_unused : 1;
    unsigned int fr_s : 1;      /* call was a calls, not callg */
    unsigned int fr_spa : 2;    /* stack pointer alignment */
    unsigned int fr_savap;      /* saved arg pointer */
    unsigned int fr_savfp;      /* saved frame pointer */
    int fr_savpc;           /* saved program counter */
#endif
#ifdef tahoe
    int fr_savpc;           /* saved program counter */
    unsigned short fr_mask;     /* register save mask */
    unsigned short fr_removed;  /* (nargs+1)*4 */
    unsigned int fr_savfp;      /* saved frame pointer */
#endif
} Stkframe;

#define regsaved(frame, n) ((frame.fr_mask&(1 << n)) != 0)

LOCAL ADDRESS fetchpc(framep)
ADDRESS *framep;
{
    register PROCESS *p;
    Stkframe sframe;
#ifdef tahoe
#define	PCREGLOC	(-1)
#else
#define	PCREGLOC	(sizeof sframe/sizeof(ADDRESS))
#endif
    ADDRESS *savfp;
    ADDRESS r;

    p = process;
    r = p->reg[PCREG];
    if (p->fp == (ADDRESS) framep) {
	return r;
    }
    savfp = (ADDRESS *) p->fp;
#ifdef tahoe
    savfp -= 2;
#endif
    dread(&sframe, savfp, sizeof(sframe));
    while (sframe.fr_savfp != (int) framep && sframe.fr_savfp != 0) {
	if (regsaved(sframe, PCREG)) {
	    dread(&r, savfp + PCREGLOC, sizeof(r));
	    r -= sizeof(char);
	}
	savfp = (ADDRESS *) sframe.fr_savfp;
#ifdef tahoe
	savfp -= 2;
#endif
	dread(&sframe, savfp, sizeof(sframe));
    }
    if (sframe.fr_savfp == 0) {
	panic("resume: can't find interpreter frame 0x%x", framep);
    }
    if (regsaved(sframe, PCREG)) {
	dread(&r, savfp + PCREGLOC, sizeof(r));
	r -= sizeof(char);
    }
    return(r);
}

#endif

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
