/*
 * Copyright (c) 1986, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_pcs.c	7.4 (Berkeley) 5/3/90
 */

#include "../kdb/defs.h"

char	*kdbNOBKPT;
char	*kdbSZBKPT;
char	*kdbEXBKPT;
char	*kdbBADMOD;

/* breakpoints */
BKPTR	kdbbkpthead;

char	*kdblp;
char	kdblastc;
extern	char *kdbmalloc();
long	kdbloopcnt;

/* sub process control */

kdbsubpcs(modif)
{
	register check, runmode;
	register BKPTR bkptr;
	register char *comptr;

	kdbloopcnt=kdbcntval;
	switch (modif) {

		/* delete breakpoint */
	case 'd': case 'D':
		if (bkptr=kdbscanbkpt((ADDR)kdbdot)) {
			bkptr->flag=0;
			return;
		}
		kdberror(kdbNOBKPT);

		/* set breakpoint */
	case 'b': case 'B':
		if (bkptr=kdbscanbkpt((ADDR)kdbdot))
			bkptr->flag=0;
		for (bkptr=kdbbkpthead; bkptr; bkptr=bkptr->nxtbkpt)
			if (bkptr->flag == 0)
				break;
		if (bkptr==0) {
			bkptr=(BKPTR)kdbmalloc(sizeof *bkptr);
			if (bkptr == (BKPTR)-1)
				kdberror(kdbSZBKPT);
			bkptr->nxtbkpt=kdbbkpthead;
			kdbbkpthead=bkptr;
		}
		bkptr->loc = kdbdot;
		bkptr->initcnt = bkptr->count = kdbcntval;
		bkptr->flag = BKPTSET;
		check=MAXCOM-1; comptr=bkptr->comm; (void) kdbrdc(); kdblp--;
		do
			*comptr++ = kdbreadchar();
		while (check-- && kdblastc!=EOR);
		*comptr=0; kdblp--;
		if (check)
			return;
		kdberror(kdbEXBKPT);

		/* single step */
	case 's': case 'S':
		runmode=SINGLE;
		break;

		/* continue */
	case 'c': case 'C':
		runmode=CONTIN;
		break;

		/* kill */
	case 'k': case 'K':
		reset(PANIC);
		/* NOTREACHED */

	default:
		kdberror(kdbBADMOD);
	}
	if (kdbloopcnt>0)
		kdbrunpcs(runmode, 0);
}
