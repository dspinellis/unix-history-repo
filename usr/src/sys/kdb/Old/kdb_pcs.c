/*
 * Copyright (c) 1986, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_pcs.c	7.3 (Berkeley) 9/2/89
 */

#include "../kdb/defs.h"

char	*NOBKPT;
char	*SZBKPT;
char	*EXBKPT;
char	*BADMOD;

/* breakpoints */
BKPTR	bkpthead;

char	*lp;
char	lastc;
extern	char *kdbmalloc();
long	loopcnt;

/* sub process control */

subpcs(modif)
{
	register check, runmode;
	register BKPTR bkptr;
	register char *comptr;

	loopcnt=cntval;
	switch (modif) {

		/* delete breakpoint */
	case 'd': case 'D':
		if (bkptr=scanbkpt((ADDR)dot)) {
			bkptr->flag=0;
			return;
		}
		error(NOBKPT);

		/* set breakpoint */
	case 'b': case 'B':
		if (bkptr=scanbkpt((ADDR)dot))
			bkptr->flag=0;
		for (bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt)
			if (bkptr->flag == 0)
				break;
		if (bkptr==0) {
			bkptr=(BKPTR)kdbmalloc(sizeof *bkptr);
			if (bkptr == (BKPTR)-1)
				error(SZBKPT);
			bkptr->nxtbkpt=bkpthead;
			bkpthead=bkptr;
		}
		bkptr->loc = dot;
		bkptr->initcnt = bkptr->count = cntval;
		bkptr->flag = BKPTSET;
		check=MAXCOM-1; comptr=bkptr->comm; (void) rdc(); lp--;
		do
			*comptr++ = readchar();
		while (check-- && lastc!=EOR);
		*comptr=0; lp--;
		if (check)
			return;
		error(EXBKPT);

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
		error(BADMOD);
	}
	if (loopcnt>0)
		runpcs(runmode, 0);
}
