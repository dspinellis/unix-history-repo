/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_runpcs.c	7.4 (Berkeley) 5/3/90
 */

#include "../kdb/defs.h"

char	*kdblp;

/* breakpoints */
BKPTR	kdbbkpthead;

char	kdblastc;

long	kdbdot;
int	kdbadrflg;
long	kdbloopcnt;
ADDR	kdbuserpc = 1;

kdbrunpcs(runmode, execsig)
{
	register BKPTR bkpt;

	if (kdbadrflg)
		kdbuserpc = kdbdot;
	if (execsig == 0)
		kdbprintf("kdb: running\n");
	if (runmode==SINGLE) {
		/*
		 * To single step, delete the
		 * breakpoints and invoke the
		 * hardware single step in the
		 * main loop.
		 */
		kdbdelbp();
		reset(SINGLE);
	}
	/*
	 * If we're currently at a breakpoint,
	 * restore the instruction and single
	 * step before continuing.  Otherwise,
	 * we can just set our breakpoints and
	 * continue.
	 */
	if (bkpt = kdbscanbkpt(kdbuserpc)) {
		kdbexecbkpt(bkpt);
		/*NOTREACHED*/
	}
	kdbsetbp();
	reset(CONTIN);
}

static	int kdbexecbkptf;

/*
 * Continue execution after a trap.
 *
 * If tracetrap is nonzero, we've entered here because of a
 * trace trap.  If we're skipping a breakpoint (execbkptf),
 * or this is the next iteration of a breakpoint, continue.
 * If this is the next iteration of a single step, do the
 * next step.  Otherwise return 1 if we stopped because
 * of a breakpoint,
 */
kdbnextpcs(tracetrap)
	int tracetrap;
{
	register BKPTR bkpt;
	short rc;

	clrsstep();			/* clear hardware single step */
	kdbdelbp();
	if (kdbexecbkptf) {
		kdbexecbkptf = 0;
		kdbrunpcs(CONTIN, 1);
		/*NOTREACHED*/
	}
	if (!tracetrap && (bkpt = kdbscanbkpt(kdbuserpc))) {
		/*
		 * Stopped at a breakpoint,
		 * execute any command.
		 */
		kdbdot = bkpt->loc;
		if (bkpt->flag == BKPTEXEC ||
		    ((bkpt->flag = BKPTEXEC) && bkpt->comm[0] != EOR &&
		    kdbcommand(bkpt->comm, ':') && --bkpt->count)) {
			kdbloopcnt++;
			kdbexecbkpt(bkpt);
		} else {
			bkpt->count = bkpt->initcnt;
			rc = 1;
		}
	} else
		rc = 0;
	if (--kdbloopcnt > 0)
		kdbrunpcs(rc ? CONTIN : SINGLE, 1);
	return (rc);
}

#define BPOUT 0
#define BPIN 1
static	int kdbbpstate = BPOUT;

kdbexecbkpt(bkptr)
	BKPTR	bkptr;
{

	kdbdelbp();
	bkptr->flag = BKPTSET;
	kdbexecbkptf++;
	reset(SINGLE);
}

BKPTR
kdbscanbkpt(addr)
	ADDR addr;
{
	register BKPTR	bkptr;

	for (bkptr = kdbbkpthead; bkptr; bkptr = bkptr->nxtbkpt)
		if (bkptr->flag && bkptr->loc == addr)
			break;
	return (bkptr);
}

kdbdelbp()
{
	register off_t a;
	register BKPTR bkptr;

	if (kdbbpstate == BPOUT)
		return;
	for (bkptr = kdbbkpthead; bkptr; bkptr = bkptr->nxtbkpt)
		if (bkptr->flag) {
			a = bkptr->loc;
			kdbput((off_t)a, ISP, (long)bkptr->ins);
		}
	kdbbpstate = BPOUT;
}

kdbsetbp()
{
	register off_t a;
	register BKPTR bkptr;

	if (kdbbpstate == BPIN)
		return;
	for (bkptr = kdbbkpthead; bkptr; bkptr = bkptr->nxtbkpt)
		if (bkptr->flag) {
			a = bkptr->loc;
			bkptr->ins = kdbget(a, ISP);
			kdbput(a, ISP, (long)SETBP(bkptr->ins));
		}
	kdbbpstate = BPIN;
}
