/*-
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 * from: Utah $Hdr: hil_subr.c 1.1 91/11/19$
 *
 *	@(#)hil_subr.c	7.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/clist.h>

#include <hp/dev/hilreg.h>

/*
 * XXX this file only exists to separate out the AT&T tainted code.
 */

/*
 * This is just a copy of the virgin q_to_b routine with minor
 * optimizations for HIL use.  It is used because we don't have
 * to raise the priority to spltty() for most of the clist manipulations.
 */
hilq_to_b(q, cp, cc)
	register struct clist *q;
	register char *cp;
{
	register struct cblock *bp;
	register int nc;
	char *acp;
	int s;
	extern char cwaiting;

	if (cc <= 0)
		return (0);
	s = splhil();
	if (q->c_cc <= 0) {
		q->c_cc = 0;
		q->c_cf = q->c_cl = NULL;
		splx(s);
		return (0);
	}
	acp = cp;

	while (cc) {
		nc = sizeof (struct cblock) - ((int)q->c_cf & CROUND);
		nc = min(nc, cc);
		nc = min(nc, q->c_cc);
		(void) bcopy(q->c_cf, cp, (unsigned)nc);
		q->c_cf += nc;
		q->c_cc -= nc;
		cc -= nc;
		cp += nc;
		if (q->c_cc <= 0) {
			bp = (struct cblock *)(q->c_cf - 1);
			bp = (struct cblock *)((int)bp & ~CROUND);
			q->c_cf = q->c_cl = NULL;
			spltty();
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			if (cwaiting) {
				wakeup(&cwaiting);
				cwaiting = 0;
			}
			break;
		}
		if (((int)q->c_cf & CROUND) == 0) {
			bp = (struct cblock *)(q->c_cf);
			bp--;
			q->c_cf = bp->c_next->c_info;
			spltty();
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			if (cwaiting) {
				wakeup(&cwaiting);
				cwaiting = 0;
			}
			splhil();
		}
	}
	splx(s);
	return (cp-acp);
}

